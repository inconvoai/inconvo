import {
  AIMessage,
  HumanMessage,
  ToolMessage,
  type BaseMessage,
  isAIMessage,
  isToolMessage,
  type MessageContentText,
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
import type { Schema } from "~/server/db/schema";
import { getPrompt } from "../utils/getPrompt";
import { databaseRetrieverAgent } from "../database";
import type { RunnableToolLike } from "@langchain/core/runnables";
import { buildTableSchemaStringFromTableSchema } from "../database/utils/schemaFormatters";
import { stringArrayToZodEnum } from "../utils/zodHelpers";
import { getAIModel } from "../utils/getAIModel";
import { inconvoResponseSchema } from "~/server/userDatabaseConnector/types";
import { tryCatchSync } from "~/server/api/utils/tryCatch";
import { inngest } from "~/server/inngest/client";
import type { Conversation } from "@prisma/client";
import { databaseRetrieverToolDescription } from "../database/utils/databaseRetrieverToolDescription";
import { InconvoSandbox } from "../utils/sandbox";

interface Chart {
  type: "bar" | "line";
  data: {
    labels: string[];
    datasets: {
      name: string;
      values: number[];
    }[];
  };
  title: string;
  xLabel: string;
  yLabel: string;
}

type Table = {
  head: string[];
  body: [][];
};

interface Answer {
  type: string;
  message: string;
  chart?: Chart;
  table?: Table;
}

function assertChartDatasetsMatchLabels(chart: Chart) {
  const labelCount = chart.data.labels.length;
  chart.data.datasets.forEach((dataset) => {
    if (dataset.values.length !== labelCount) {
      throw new Error(
        `Chart dataset "${dataset.name}" must include ${labelCount} values.`
      );
    }
  });
}

interface QuestionAgentParams {
  schema: Schema;
  connectorUrl: string;
  connectorSigningKey: string;
  checkpointer: PostgresSaver | MemorySaver;
  conversation: Conversation;
}

const messageReducer = (
  x: BaseMessage[] | null,
  y: BaseMessage[] | null
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
    if (isToolMessage(msg) && msg.tool_call_id) {
      messageMap.set(msg.tool_call_id, msg);
    }
  });

  // Second pass: find AI messages with the specified tool calls and their responses
  messages.forEach((msg) => {
    if (isAIMessage(msg)) {
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
 * Checks if any message in the array contains database retriever tool calls
 */
function hasDatabaseRetrieverCall(messages: BaseMessage[]): boolean {
  return messages.some(
    (msg) =>
      isAIMessage(msg) &&
      (msg.tool_calls?.some((call) => call.name === "databaseRetriever") ??
        msg.invalid_tool_calls?.some(
          (call) => call.name === "databaseRetriever"
        ))
  );
}

function extractTextFromMessage(
  message: BaseMessage | null | undefined
): string[] {
  if (!message) return [""];

  const { content } = message;

  if (typeof content === "string") {
    return [content];
  }

  if (!Array.isArray(content)) {
    return [""];
  }

  const textMessages = content
    .filter((m): m is MessageContentText => m.type === "text")
    .map((tm) => tm.text);

  return textMessages;
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
            if (isAIMessage(msg)) {
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
            if (isToolMessage(msg) && msg.tool_call_id) {
              if (dbToolCallIds.has(msg.tool_call_id)) return false;
            }
            // Remove AI messages that contain databaseRetriever tool calls
            if (isAIMessage(msg)) {
              const hasDbCall =
                msg.tool_calls?.some((c) => c.name === "databaseRetriever") ??
                msg.invalid_tool_calls?.some(
                  (c) => c.name === "databaseRetriever"
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
  });

  const tools: (StructuredToolInterface | DynamicTool | RunnableToolLike)[] =
    [];
  const toolNode = new ToolNode(tools);

  const model = getAIModel("azure:gpt-5", {
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
              connectorUrl: params.connectorUrl,
              connectorSigningKey: params.connectorSigningKey,
            })
          ).invoke({});
          const toolCallId = config.toolCall?.id;
          if (!toolCallId) {
            throw new Error(
              "Tool call ID is missing in ToolRunnableConfig. Cannot create ToolMessage without a valid tool_call_id."
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
                      // verification: databaseRetrieverResponse.reason,
                      query: databaseRetrieverResponse.databaseResponse.query,
                      data: databaseRetrieverResponse.databaseResponse.response,
                    },
                    null,
                    2
                  ),
                  tool_call_id: toolCallId,
                }),
              ],
            },
          });
        } catch (e) {
          return {
            error: `Calling tool with arguments:\n\n${JSON.stringify(
              input
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
      }
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
            stringArrayToZodEnum(tables).describe("The name of the table")
          ),
        }),
      }
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
          }))
        );

        const describeResult = await sandbox.describeFiles(
          uploadResult.files.map((file) => file.name)
        );
        const toolCallId = config.toolCall?.id;
        if (!toolCallId) {
          throw new Error(
            "Tool call ID is missing in ToolRunnableConfig. Cannot create ToolMessage without a valid tool_call_id."
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
      }
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
              "Tool call ID is missing in ToolRunnableConfig. Cannot create ToolMessage without a valid tool_call_id."
            );
          }
          return new Command({
            update: {
              sandboxUsed: true,
              messages: [
                new ToolMessage({
                  status: "success",
                  name: "executePythonCode",
                  content: executionResult.output,
                  tool_call_id: toolCallId,
                }),
              ],
            },
          });
        } catch (e) {
          return `Calling tool with arguments:\n\n${JSON.stringify(
            input
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
      }
    );
    tools.push(executePythonCode);

    return {};
  }

  const shouldContinue = (state: typeof AgentState.State) => {
    const { messages } = state;
    const lastMessage = messages?.at(-1) as AIMessage;
    if (
      isAIMessage(lastMessage) &&
      (!lastMessage.tool_calls || lastMessage.tool_calls.length === 0)
    ) {
      return "formatResponse";
    } else {
      return "continue";
    }
  };

  async function callModel(state: typeof AgentState.State) {
    const prompt = await getPrompt("inconvo_agent_gpt5_dev:576b20e6");
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

    // Extract potential JSON responses from the last message
    const potentialJsonResponses = extractTextFromMessage(
      state.messages?.at(-1)
    );

    // Filter to only valid JSON that matches inconvoResponseSchema
    type InconvoResponse = z.infer<typeof inconvoResponseSchema>;
    const validResponses = potentialJsonResponses
      .map((jsonString: string) => {
        const { data } = tryCatchSync(
          () => JSON.parse(jsonString) as unknown
        ) as { data: unknown; error: Error | null };
        return data;
      })
      .filter(
        (data: unknown): data is NonNullable<typeof data> =>
          data !== null && data !== undefined
      )
      .map((data: unknown) => inconvoResponseSchema.safeParse(data))
      .filter(
        (result): result is z.SafeParseSuccess<InconvoResponse> =>
          result.success
      )
      .map((result) => result.data);

    const stateMessages = state.messages ?? [];
    const databaseRelatedMessages = extractToolRelatedMessages(
      stateMessages,
      "databaseRetriever"
    );
    const getSchemaRelatedMessages = extractToolRelatedMessages(
      stateMessages,
      "getSchemasForTables"
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

      if (selectedResponse.type === "chart" && selectedResponse.chart) {
        assertChartDatasetsMatchLabels(selectedResponse.chart);
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

    const outputFormatterPrompt = await getPrompt("format_response:249747a7");

    const variantUnion = z.discriminatedUnion("type", [
      z.object({
        type: z.literal("text"),
        message: z.string().describe("Plain text answer"),
      }),
      z.object({
        type: z.literal("chart"),
        message: z
          .string()
          .describe(
            "Explanation accompanying the chart highlighting key insights"
          ),
        chart: z
          .object({
            type: z.enum(["line", "bar"]).describe("Chart type"),
            data: z
              .object({
                labels: z
                  .array(z.string())
                  .min(1)
                  .describe("Ordered labels for the x axis"),
                datasets: z
                  .array(
                    z
                      .object({
                        name: z
                          .string()
                          .describe(
                            "Name of the dataset to render in the chart"
                          ),
                        values: z
                          .array(z.number())
                          .describe(
                            "Ordered y axis values, each index aligns with the labels array"
                          ),
                      })
                      .strict()
                  )
                  .min(1)
                  .describe(
                    "Datasets to plot, aligned by index to the labels array"
                  ),
              })
              .strict(),
            title: z.string(),
            xLabel: z.string(),
            yLabel: z.string(),
          })
          .strict(),
      }),
      z.object({
        type: z.literal("table"),
        message: z.string().describe("Summary of what the table shows"),
        table: z.object({
          head: z.array(z.string()).min(1).describe("Column headers"),
          body: z
            .array(z.array(z.string()).min(1))
            .min(1)
            .describe("Table rows aligned with headers"),
        }),
      }),
    ]);

    const wrappedSchema = z.object({
      data: variantUnion.describe(
        "Discriminated union payload. Use type to decide which shape to return."
      ),
    });
    const model_4_1 = getAIModel("azure:gpt-4.1");
    const outputFormatterSchema = model_4_1.withStructuredOutput(
      wrappedSchema,
      {
        strict: true,
        method: "jsonSchema",
      }
    );

    // Fallback to LLM formatter if no valid responses found
    const messageToFormat = potentialJsonResponses.join("\n") ?? "";

    const wrapped = await outputFormatterPrompt
      .pipe(outputFormatterSchema)
      .invoke({
        messageToFormat,
      });

    const response = wrapped.data as z.infer<typeof variantUnion>;

    if (response.type === "chart" && response.chart) {
      assertChartDatasetsMatchLabels(response.chart);
    }

    return {
      answer: response,
      chatHistory: [
        new HumanMessage(state.userQuestion),
        ...getSchemaRelatedMessages,
        ...databaseRelatedMessages,
        new AIMessage(JSON.stringify(response, null, 2)),
      ],
    };
  }

  async function titleConversation(state: typeof AgentState.State) {
    if (params.conversation.title) {
      return {};
    }

    await inngest.send({
      name: "conversation/title",
      data: {
        conversationId: params.conversation.id,
        encrypted: { message: state.userQuestion },
      },
    });
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
