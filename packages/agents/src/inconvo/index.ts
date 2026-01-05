import {
  AIMessage,
  HumanMessage,
  SystemMessage,
  ToolMessage,
  type BaseMessage,
} from "@langchain/core/messages";
import { ToolNode } from "@langchain/langgraph/prebuilt";
import {
  StateGraph,
  type MemorySaver,
  Annotation,
  END,
  START,
  Command,
  getCurrentTaskInput,
} from "@langchain/langgraph";
import {
  type DynamicTool,
  tool,
  type StructuredToolInterface,
  type ToolRunnableConfig,
} from "@langchain/core/tools";
import { z } from "zod";
import type { PostgresSaver } from "@langchain/langgraph-checkpoint-postgres";
import type { Schema, DatabaseConnector } from "@repo/types";
import { getPrompt } from "../utils/getPrompt";
import { databaseRetrieverAgent } from "../database";
import type { RunnableToolLike } from "@langchain/core/runnables";
import { buildTableSchemaStringFromTableSchema } from "../database/utils/schemaFormatters";
import { stringArrayToZodEnum } from "../utils/zodHelpers";
import { getAIModel } from "../utils/getAIModel";
import { inconvoResponseSchema } from "@repo/types";
import { tryCatchSync } from "../utils/tryCatch";
import type { Conversation } from "@repo/types";
import { databaseRetrieverToolDescription } from "../database/utils/databaseRetrieverToolDescription";
import { InconvoSandbox } from "../utils/sandbox";
import { extractTextFromMessage } from "../utils/langchainMessageUtils";

import type { VegaLiteSpec } from "@repo/types";
import * as vegaLite from "vega-lite";
import * as vega from "vega";

type Table = {
  head: string[];
  body: [][];
};

interface Answer {
  type: string;
  message: string;
  spec?: VegaLiteSpec;
  table?: Table;
}

interface QuestionAgentParams {
  schema: Schema;
  connector: DatabaseConnector;
  checkpointer: PostgresSaver | MemorySaver;
  conversation: Conversation;
  /** Optional callback to trigger conversation titling (e.g., via Inngest in platform) */
  onTitleConversation?: (
    conversationId: string,
    message: string,
  ) => Promise<void>;
}

const messageReducer = (
  x: BaseMessage[] | null,
  y: BaseMessage[] | null,
): BaseMessage[] => {
  const emptyArray: BaseMessage[] = [];
  if (!y) {
    return emptyArray;
  } else if (!x) {
    return emptyArray.concat(y);
  }
  if (x && y) {
    return x.concat(y);
  }
  return emptyArray;
};

/**
 * Extracts tool calls (for a specific tool name) and their corresponding tool messages
 * from a list of messages. Returns an ordered list containing each AI message that
 * invoked the tool followed immediately by its matching ToolMessage (if present).
 */
function extractToolRelatedMessages(messages: BaseMessage[], toolName: string) {
  const relatedMessages: BaseMessage[] = [];
  const messageMap = new Map<string, ToolMessage>();

  // First pass: map tool_call_id -> ToolMessage for O(1) lookup
  messages.forEach((msg) => {
    if (ToolMessage.isInstance(msg) && msg.tool_call_id) {
      messageMap.set(msg.tool_call_id, msg);
    }
  });

  // Second pass: find AI messages with the specified tool calls and their responses
  messages.forEach((msg) => {
    if (AIMessage.isInstance(msg)) {
      const matchingCalls = [
        ...(msg.tool_calls ?? []),
        ...(msg.invalid_tool_calls ?? []),
      ].filter((call) => call.name === toolName);

      if (matchingCalls.length > 0) {
        relatedMessages.push(msg);
        matchingCalls.forEach((call) => {
          if (call.id) {
            const toolMessage = messageMap.get(call.id);
            if (toolMessage) relatedMessages.push(toolMessage);
          }
        });
      }
    }
  });

  return relatedMessages;
}

/**
 * Validates a Vega-Lite spec by compiling it to Vega and parsing.
 * This catches both structural issues and invalid expression functions.
 * Returns an array of error messages if validation fails.
 */
function validateVegaLiteSpec(
  response: z.infer<typeof inconvoResponseSchema>,
): string[] {
  if (response.type !== "chart") {
    return [];
  }

  // First, try to compile the Vega-Lite spec to a Vega spec.
  let compiled;
  try {
    compiled = vegaLite.compile(response.spec as vegaLite.TopLevelSpec);
  } catch (error) {
    const message = error instanceof Error ? error.message : String(error);
    return [`Vega-Lite compilation error: ${message}`];
  }

  // Then, parse the Vega spec to validate expressions (e.g., invalid functions like dayofweek).
  try {
    vega.parse(compiled.spec);
    return [];
  } catch (error) {
    const message = error instanceof Error ? error.message : String(error);
    return [`Vega parsing error: ${message}`];
  }
}

/**
 * Checks if any message in the array contains database retriever tool calls
 */
function hasDatabaseRetrieverCall(messages: BaseMessage[]): boolean {
  return messages.some(
    (msg) =>
      AIMessage.isInstance(msg) &&
      (msg.tool_calls?.some((call) => call.name === "databaseRetriever") ??
        msg.invalid_tool_calls?.some(
          (call) => call.name === "databaseRetriever",
        )),
  );
}

export async function inconvoAgent(params: QuestionAgentParams) {
  const AgentState = Annotation.Root({
    userQuestion: Annotation<string>({
      reducer: (x, y) => y,
      default: () => "",
    }),
    runId: Annotation<string>({
      reducer: (x, y) => y,
      default: () => "",
    }),
    sandboxUsed: Annotation<boolean>({
      reducer: (x, y) => x || y,
      default: () => false,
    }),
    // Messages in the current run
    messages: Annotation<BaseMessage[] | null>({
      reducer: (x, y) => messageReducer(x, y),
      default: () => [],
    }),
    // The QA message pairs from previous runs
    chatHistory: Annotation<BaseMessage[]>({
      reducer: (x, y) => {
        // When new messages contain database retriever calls,
        // we filter out ONLY the tool messages and AI messages associated
        // with databaseRetriever tool calls (instead of removing every tool message)
        if (hasDatabaseRetrieverCall(y)) {
          // 1. Collect all databaseRetriever tool_call ids present in the current history
          const dbToolCallIds = new Set<string>();
          x.forEach((msg) => {
            if (AIMessage.isInstance(msg)) {
              const calls = [
                ...(msg.tool_calls ?? []),
                ...(msg.invalid_tool_calls ?? []),
              ];
              calls.forEach((call) => {
                if (call.name === "databaseRetriever" && call.id) {
                  dbToolCallIds.add(call.id);
                }
              });
            }
          });

          const filteredHistory = x.filter((msg) => {
            // Remove only tool messages whose tool_call_id maps to a databaseRetriever call
            if (ToolMessage.isInstance(msg) && msg.tool_call_id) {
              if (dbToolCallIds.has(msg.tool_call_id)) return false;
            }
            // Remove AI messages that contain databaseRetriever tool calls
            if (AIMessage.isInstance(msg)) {
              const hasDbCall =
                msg.tool_calls?.some((c) => c.name === "databaseRetriever") ??
                msg.invalid_tool_calls?.some(
                  (c) => c.name === "databaseRetriever",
                );
              if (hasDbCall) return false;
            }
            return true;
          });
          return filteredHistory.concat(y);
        }

        // No database calls in new messages, just append
        return x.concat(y);
      },
      default: () => [],
    }),
    answer: Annotation<Answer>({
      reducer: (x, y) => y,
      default: () => ({
        type: "text",
        message: "",
      }),
    }),
    databaseRetrieverResults: Annotation<
      { query?: string; data?: Record<string, unknown> }[] | undefined
    >({
      reducer: (x, y) => (x ? x.concat(y ?? []) : y),
      default: () => undefined,
    }),
    // Context about the request to pass to the database retriever
    requestContext: Annotation<Record<string, string | number>>({
      reducer: (x, y) => y,
      default: () =>
        params.conversation.requestContext as Record<string, string | number>,
    }),
    error: Annotation<Record<string, unknown> | undefined>({
      reducer: (x, y) => y,
      default: () => undefined,
    }),
    formatAttempts: Annotation<number>({
      reducer: (x, y) => x + y,
      default: () => 0,
    }),
  });

  const tools: (StructuredToolInterface | DynamicTool | RunnableToolLike)[] =
    [];
  const toolNode = new ToolNode(tools);

  const model = getAIModel("azure:gpt-5.1", {
    reasoning: { effort: "low", summary: "detailed" },
  });

  const tableContext = params.schema
    .filter((table) => table.context)
    .map((table) => `Name: ${table.name},\n Context: ${table.context}`)
    .join("\n");

  function resetState(_state: typeof AgentState.State) {
    // We only have to preserve the chat history from the checkpointer
    // and also the database retriever results if any
    // The user question will be overwritten by start node because it the graph
    // Is invoked with a user question
    return {
      messages: null,
      error: undefined,
      answer: {
        type: "text",
        message: "",
      },
      formatAttempts: -_state.formatAttempts, // Reset to 0 via reducer
    };
  }

  function buildTools(state: typeof AgentState.State) {
    const tables = params.schema.map((table) => table.name);

    const databaseRetriever = tool(
      async (input: { question: string }, config: ToolRunnableConfig) => {
        try {
          const databaseRetrieverResponse = await (
            await databaseRetrieverAgent({
              userQuestion: input.question,
              schema: params.schema,
              requestContext: state.requestContext,
              connector: params.connector,
            })
          ).invoke({});
          const toolCallId = config.toolCall?.id;
          if (!toolCallId) {
            throw new Error(
              "Tool call ID is missing in ToolRunnableConfig. Cannot create ToolMessage without a valid tool_call_id.",
            );
          }
          return new Command({
            update: {
              databaseRetrieverResults: [
                {
                  query: databaseRetrieverResponse.databaseResponse.query,
                  data: databaseRetrieverResponse.databaseResponse.response,
                },
              ],
              messages: [
                new ToolMessage({
                  status: "success",
                  name: "databaseRetriever",
                  content: JSON.stringify(
                    {
                      query: databaseRetrieverResponse.databaseResponse.query,
                      data: databaseRetrieverResponse.databaseResponse.response,
                      ...(databaseRetrieverResponse.databaseResponse.warning
                        ? {
                            warning:
                              databaseRetrieverResponse.databaseResponse
                                .warning,
                          }
                        : {}),
                    },
                    null,
                    2,
                  ),
                  tool_call_id: toolCallId,
                }),
              ],
            },
          });
        } catch (e) {
          return {
            error: `Calling tool with arguments:\n\n${JSON.stringify(
              input,
            )}\n\nraised the following error:\n\n${
              e instanceof Error ? e.message : String(e)
            }`,
          };
        }
      },
      {
        name: "databaseRetriever",
        description: databaseRetrieverToolDescription,
        schema: z.object({
          question: z.string().describe("The question to ask the database"),
        }),
      },
    );
    tools.push(databaseRetriever);

    const getSchemasForTables = tool(
      async (input: { tables: string[] }) => {
        const schemaStrings = input.tables.map((tableName) => {
          const table = params.schema.find((s) => s.name === tableName);
          if (!table) {
            throw new Error(`Table ${tableName} not found`);
          }
          return buildTableSchemaStringFromTableSchema(table);
        });
        return schemaStrings.join("\n\n---\n\n");
      },
      {
        name: "getSchemasForTables",
        description: "Get the schemas for a list of tables (one or more).",
        schema: z.object({
          tables: z.array(
            stringArrayToZodEnum(tables).describe("The name of the table"),
          ),
        }),
      },
    );
    tools.push(getSchemasForTables);

    const uploadDataToPythonSandbox = tool(
      async (_input, config: ToolRunnableConfig) => {
        const currentState = getCurrentTaskInput<typeof AgentState.State>();
        const jsonResultsToUploadToSandbox =
          currentState.databaseRetrieverResults ?? [];
        if (
          !jsonResultsToUploadToSandbox ||
          !Array.isArray(jsonResultsToUploadToSandbox) ||
          jsonResultsToUploadToSandbox.length === 0
        ) {
          return "No database retriever results available to upload to the Python sandbox. Please run a database retrieval first.";
        }
        const sandbox = new InconvoSandbox({
          sandboxId: currentState.runId,
        });

        const uploadResult = await sandbox.uploadFiles(
          jsonResultsToUploadToSandbox.map((result, index) => ({
            name: `retriever_result_${index + 1}.json`,
            content: JSON.stringify(result, null, 2),
          })),
        );

        const describeResult = await sandbox.describeFiles(
          uploadResult.files.map((file) => file.name),
        );
        const toolCallId = config.toolCall?.id;
        if (!toolCallId) {
          throw new Error(
            "Tool call ID is missing in ToolRunnableConfig. Cannot create ToolMessage without a valid tool_call_id.",
          );
        }
        return new Command({
          update: {
            sandboxUsed: true,
            messages: [
              new ToolMessage({
                status: "success",
                name: "uploadDataToPythonSandbox",
                content: JSON.stringify(describeResult),
                tool_call_id: toolCallId,
              }),
            ],
          },
        });
      },
      {
        name: "uploadDataToPythonSandbox",
        description:
          "Uploads database retriever results from this conversation to a Python sandbox environment. " +
          "Returns a summary of the uploaded data including DataFrame descriptions and filenames. " +
          "This tool MUST be called at least once before using 'executePythonCode' to ensure data is available in the sandbox. " +
          "No input parameters required - automatically uses data from current conversation state.",
        schema: z.object({}),
      },
    );
    tools.push(uploadDataToPythonSandbox);

    const executePythonCode = tool(
      async (input: { code: string }, config: ToolRunnableConfig) => {
        try {
          const currentState = getCurrentTaskInput<typeof AgentState.State>();
          const sandbox = new InconvoSandbox({
            sandboxId: currentState.runId,
          });
          const executionResult = await sandbox.executeCode(input.code);
          const toolCallId = config.toolCall?.id;
          if (!toolCallId) {
            throw new Error(
              "Tool call ID is missing in ToolRunnableConfig. Cannot create ToolMessage without a valid tool_call_id.",
            );
          }
          return new Command({
            update: {
              sandboxUsed: true,
              messages: [
                new ToolMessage({
                  status: "success",
                  name: "executePythonCode",
                  content: JSON.stringify(executionResult, null, 2),
                  tool_call_id: toolCallId,
                }),
              ],
            },
          });
        } catch (e) {
          return `Calling tool with arguments:\n\n${JSON.stringify(
            input,
          )}\n\nraised the following error:\n\n${
            e instanceof Error ? e.message : String(e)
          }`;
        }
      },
      {
        name: "executePythonCode",
        description:
          "Executes Python 3 code in a sandboxed environment for calculations and data analysis.\n\n" +
          "IMPORTANT: You must call 'uploadDataToPythonSandbox' tool first to make data available.\n\n" +
          "Only printed output is returned, so print any data you need to inspect.\n\n" +
          "Available libraries: pandas, numpy, json\n\n" +
          "Data files uploaded via 'uploadDataToPythonSandbox' are available as 'retriever_result_N.json'.\n\n" +
          "Example code to load and analyze data:\n" +
          "```python\n" +
          "import pandas as pd\n" +
          "import json\n\n" +
          "# Load JSON file\n" +
          "with open('retriever_result_1.json', 'r') as f:\n" +
          "    data = json.load(f)\n\n" +
          "# Normalize to DataFrame\n" +
          "if 'data' in data:\n" +
          "    df = pd.json_normalize(data['data'])\n" +
          "else:\n" +
          "    df = pd.json_normalize(data if isinstance(data, list) else [data])\n\n" +
          "# Analyze\n" +
          "print(df.head())\n" +
          "print(df.describe())\n" +
          "```",
        schema: z.object({
          code: z
            .string()
            .describe("The Python 3 code to execute in the sandbox"),
        }),
      },
    );
    tools.push(executePythonCode);

    return {};
  }

  const shouldContinue = (state: typeof AgentState.State) => {
    const { messages } = state;
    const lastMessage = messages?.at(-1) as AIMessage;
    if (
      AIMessage.isInstance(lastMessage) &&
      (!lastMessage.tool_calls || lastMessage.tool_calls.length === 0)
    ) {
      return "formatResponse";
    } else {
      return "continue";
    }
  };

  async function callModel(state: typeof AgentState.State) {
    const prompt = await getPrompt("inconvo_agent_gpt5_dev:c3dc23fe");
    const tables = params.schema.map((table) => table.name);
    const response = await prompt.pipe(model.bindTools(tools)).invoke({
      tables,
      tableContext: tableContext,
      chatHistory: state.chatHistory,
      date: new Date().toISOString(),
      requestContext: JSON.stringify(state.requestContext),
      userQuestion: new HumanMessage(state.userQuestion),
      messages: state.messages,
    });
    return { messages: [response] };
  }

  async function formatResponse(state: typeof AgentState.State) {
    if (state.sandboxUsed) {
      const sandbox = new InconvoSandbox({
        sandboxId: state.runId,
      });
      await sandbox.destroySandbox();
    }

    const lastAiMessage =
      state.messages
        ?.slice()
        .reverse()
        .find((msg) => AIMessage.isInstance(msg)) ?? new AIMessage("");

    // Extract potential JSON responses from the last message
    const potentialJsonResponses = extractTextFromMessage(lastAiMessage);

    // Try to parse and validate JSON responses against inconvoResponseSchema
    type InconvoResponse = z.infer<typeof inconvoResponseSchema>;
    const parseResults: {
      valid: InconvoResponse[];
      errors: string[];
    } = { valid: [], errors: [] };

    for (const jsonString of potentialJsonResponses) {
      const { data: parsed, error: parseError } = tryCatchSync(
        () => JSON.parse(jsonString) as unknown,
      ) as { data: unknown; error: Error | null };

      if (parseError) {
        parseResults.errors.push(`JSON parse error: ${parseError.message}`);
        continue;
      }

      // Reject legacy literal to force the model to emit the supported type
      if (
        parsed &&
        typeof parsed === "object" &&
        "type" in parsed &&
        (parsed as Record<string, unknown>).type === "vega"
      ) {
        parseResults.errors.push(
          'Legacy output type "vega" is no longer accepted. Please respond with type "chart" and include the Vega-Lite spec in "spec".',
        );
        continue;
      }

      const zodResult = inconvoResponseSchema.safeParse(parsed);
      if (zodResult.success) {
        // Additional validation for vega specs
        const vegaErrors = validateVegaLiteSpec(zodResult.data);
        if (vegaErrors.length > 0) {
          parseResults.errors.push(...vegaErrors);
        } else {
          parseResults.valid.push(zodResult.data);
        }
      } else {
        const formattedErrors = zodResult.error.issues
          .map((issue) => `  - ${issue.path.join(".")}: ${issue.message}`)
          .join("\n");
        parseResults.errors.push(
          `Schema validation failed:\n${formattedErrors}`,
        );
      }
    }

    const validResponses = parseResults.valid;

    const stateMessages = state.messages ?? [];
    const databaseRelatedMessages = extractToolRelatedMessages(
      stateMessages,
      "databaseRetriever",
    );
    const getSchemaRelatedMessages = extractToolRelatedMessages(
      stateMessages,
      "getSchemasForTables",
    );

    if (validResponses.length > 0) {
      // Prioritize by type: chart > table > text
      const priorityOrder = ["chart", "table", "text"] as const;
      let selectedResponse = validResponses[0]!;

      for (const type of priorityOrder) {
        const match = validResponses.find((r) => r.type === type);
        if (match) {
          selectedResponse = match;
          break;
        }
      }

      return {
        answer: selectedResponse,
        chatHistory: [
          new HumanMessage(state.userQuestion),
          ...getSchemaRelatedMessages,
          ...databaseRelatedMessages,
          new AIMessage(JSON.stringify(selectedResponse, null, 2)),
        ],
      };
    }

    // Validation failed - check if we've exceeded retry limit
    const maxFormatAttempts = 2;
    if (state.formatAttempts >= maxFormatAttempts) {
      // Bail out with raw text from the last AI message
      let fallbackMessage = "Sorry, I was unable to format a response.";

      const jsonString = extractTextFromMessage(lastAiMessage).join("\n");
      const { data: parsed, error: parseError } = tryCatchSync(
        () => JSON.parse(jsonString) as unknown,
      ) as { data: unknown; error: Error | null };

      if (
        !parseError &&
        typeof parsed === "object" &&
        parsed !== null &&
        "message" in parsed &&
        typeof parsed.message === "string"
      ) {
        fallbackMessage = parsed.message;
      }

      return {
        answer: {
          type: "text" as const,
          message: fallbackMessage,
        },
        chatHistory: [
          new HumanMessage(state.userQuestion),
          ...getSchemaRelatedMessages,
          ...databaseRelatedMessages,
          new AIMessage(fallbackMessage),
        ],
      };
    }

    // Route back to agent with error details
    const validationErrors = parseResults.errors.join("\n");
    const errorMessage = `Your response could not be parsed. Please fix the following issues and respond with valid JSON:\n\n${validationErrors}`;

    return new Command({
      goto: "inconvo_agent",
      update: {
        messages: [new SystemMessage(errorMessage)],
        formatAttempts: 1,
      },
    });
  }

  async function titleConversation(state: typeof AgentState.State) {
    if (params.conversation.title) {
      return {};
    }

    if (params.onTitleConversation) {
      await params.onTitleConversation(
        params.conversation.id,
        state.userQuestion,
      );
    }
    return {};
  }

  const workflow = new StateGraph(AgentState)
    .addNode("reset_state", resetState)
    .addNode("title_conversation", titleConversation)
    .addNode("build_tools", buildTools)
    .addNode("inconvo_agent", callModel)
    .addNode("inconvo_agent_tools", toolNode)
    .addNode("format_response", formatResponse)
    .addEdge(START, "reset_state")
    .addEdge("reset_state", "build_tools")
    .addEdge("reset_state", "title_conversation")
    .addEdge("build_tools", "inconvo_agent")
    .addConditionalEdges("inconvo_agent", shouldContinue, {
      continue: "inconvo_agent_tools",
      formatResponse: "format_response",
    })
    .addEdge("inconvo_agent_tools", "inconvo_agent")
    .addEdge("format_response", END);

  const graph = workflow.compile({ checkpointer: params.checkpointer });

  return { graph };
}
