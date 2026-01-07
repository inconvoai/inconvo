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
import { v4 as uuidv4 } from "uuid";
import type { PostgresSaver } from "@langchain/langgraph-checkpoint-postgres";
import type { Schema, DatabaseConnector } from "@repo/types";
import { getPrompt } from "../utils/getPrompt";
import { databaseRetrieverAgent } from "../database";
import type { RunnableToolLike } from "@langchain/core/runnables";
import { buildTableSchemaStringFromTableSchema } from "../database/utils/schemaFormatters";
import { stringArrayToZodEnum } from "../utils/zodHelpers";
import { getAIModel } from "../utils/getAIModel";
import { buildPromptCacheKey } from "../utils/promptCacheKey";
import { inconvoResponseSchema } from "@repo/types";
import { tryCatchSync } from "../utils/tryCatch";
import type { Conversation } from "@repo/types";
import { databaseRetrieverToolDescription } from "../database/utils/databaseRetrieverToolDescription";
import { createSandboxClientFromEnv } from "../utils/sandbox";
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

interface AvailableDataset {
  name: string;
  targetPath: string;
  schema?: string[];
  notes?: string;
}

interface QuestionAgentParams {
  schema: Schema;
  connector: DatabaseConnector;
  checkpointer: PostgresSaver | MemorySaver;
  conversation: Conversation;
  orgId: string;
  agentId: string;
  /** Request context path for mounting datasets bucket (e.g., "organisationId=1"). Can be empty string for root. */
  requestContextPath: string;
  /** Available dataset files with metadata for agent context */
  availableDatasets?: AvailableDataset[];
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

  const promptCacheKey = buildPromptCacheKey({
    agentId: params.agentId,
    requestContext: params.conversation.requestContext as Record<
      string,
      string | number
    >,
  });

  const model = getAIModel("azure:gpt-5.2", {
    promptCacheKey,
    // reasoning: { effort: "low", summary: "detailed" },
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
              agentId: params.agentId,
              connector: params.connector,
            })
          ).invoke({});

          // Start sandbox and upload results
          const sandboxClient = createSandboxClientFromEnv({
            orgId: params.orgId,
            agentId: params.agentId,
          });
          const sandboxSession = sandboxClient.sandbox({
            conversationId: params.conversation.id,
            requestContextPath: params.requestContextPath,
          });
          await sandboxSession.start();

          const fullData = databaseRetrieverResponse.databaseResponse.response;
          const query = databaseRetrieverResponse.databaseResponse.query;

          // Upload full data to sandbox s3 storage
          const resultData = { query, data: fullData };
          const sandboxFileName = `${uuidv4()}.json`;
          const jsonString = JSON.stringify(resultData, null, 2);
          const base64Content = Buffer.from(jsonString, "utf-8").toString("base64");
          await sandboxClient.uploadConversationData({
            conversationId: params.conversation.id,
            requestContextPath: params.requestContextPath,
            files: [
              {
                name: sandboxFileName,
                content: base64Content,
                contentType: "application/json",
              },
            ],
          });

          // Truncate data for LLM response (50 rows max)
          const DATA_PREVIEW_LIMIT = 50;
          const isArray = Array.isArray(fullData);
          const totalRows = isArray ? fullData.length : 1;
          const wasTruncated = isArray && totalRows > DATA_PREVIEW_LIMIT;
          const truncatedData = isArray
            ? fullData.slice(0, DATA_PREVIEW_LIMIT)
            : fullData;

          // Build note if data was truncated or hit 1000-row DB limit
          const hit1000Limit =
            !!databaseRetrieverResponse.databaseResponse.warning;
          let note: string | undefined;
          if (wasTruncated || hit1000Limit) {
            const parts: string[] = [];
            if (wasTruncated) {
              parts.push(
                `Showing ${DATA_PREVIEW_LIMIT} of ${totalRows} rows. Full results available in sandbox file: ${sandboxFileName}`,
              );
            }
            if (hit1000Limit) {
              parts.push(
                `Query hit the 1,000-row database limit. The sandbox file contains up to 1,000 rows.`,
              );
            }
            note = parts.join(" ");
          }

          const toolCallId = config.toolCall?.id;
          if (!toolCallId) {
            throw new Error(
              "Tool call ID is missing in ToolRunnableConfig. Cannot create ToolMessage without a valid tool_call_id.",
            );
          }

          // Build LLM response with truncated data
          const llmResponse: Record<string, unknown> = {
            query,
            data: truncatedData,
            sandboxFile: sandboxFileName,
          };
          if (note) {
            llmResponse.note = note;
          }

          return new Command({
            update: {
              databaseRetrieverResults: [resultData],
              messages: [
                new ToolMessage({
                  status: "success",
                  name: "databaseRetriever",
                  content: JSON.stringify(llmResponse, null, 2),
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

    const executePythonCode = tool(
      async (input: { code: string }, config: ToolRunnableConfig) => {
        try {
          const sandboxClient = createSandboxClientFromEnv({
            orgId: params.orgId,
            agentId: params.agentId,
          });
          const sandboxSession = sandboxClient.sandbox({
            conversationId: params.conversation.id,
            requestContextPath: params.requestContextPath,
          });
          const executionResult = await sandboxSession.executeCode(input.code);
          const toolCallId = config.toolCall?.id;
          if (!toolCallId) {
            throw new Error(
              "Tool call ID is missing in ToolRunnableConfig. Cannot create ToolMessage without a valid tool_call_id.",
            );
          }
          return new Command({
            update: {
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
          "Executes Python 3 code in a sandboxed environment for intermediate calculations and data analysis.\n\n" +
          "**Use this for intermediate analysis.** For final responses to the user, use `generateResponse` instead.\n\n" +
          "**File paths (IMPORTANT - always use full paths):**\n" +
          "- Database results: `/conversation_data/{sandboxFile}`\n" +
          "- Static datasets: `/datasets/{filename}`\n\n" +
          "Only printed output is returned, so print any data you need to inspect.\n\n" +
          "**Available libraries:** pandas, numpy, json, altair\n\n" +
          "**Example:**\n" +
          "```python\n" +
          "import pandas as pd\n" +
          "import json\n\n" +
          "# IMPORTANT: Use full path /conversation_data/ + sandboxFile\n" +
          "with open('/conversation_data/{sandboxFile}', 'r') as f:\n" +
          "    data = json.load(f)\n\n" +
          "df = pd.DataFrame(data['data'])\n" +
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

    const generateResponse = tool(
      async (input: { code: string }, config: ToolRunnableConfig) => {
        try {
          const sandboxClient = createSandboxClientFromEnv({
            orgId: params.orgId,
            agentId: params.agentId,
          });
          const sandboxSession = sandboxClient.sandbox({
            conversationId: params.conversation.id,
            requestContextPath: params.requestContextPath,
          });

          const executionResult = await sandboxSession.executeCode(input.code);

          if (!executionResult.success) {
            return `Code execution failed:\n${executionResult.error}\n\nOutput:\n${executionResult.output}`;
          }

          // Parse stdout as JSON response
          const outputText = executionResult.output.trim();
          let parsedResponse: unknown;
          try {
            parsedResponse = JSON.parse(outputText);
          } catch {
            return `Failed to parse output as JSON. Make sure your code prints valid JSON.\n\nOutput received:\n${outputText}`;
          }

          // Validate against inconvoResponseSchema
          const validated = inconvoResponseSchema.safeParse(parsedResponse);
          if (!validated.success) {
            const errors = validated.error.issues
              .map((i) => `- ${i.path.join(".")}: ${i.message}`)
              .join("\n");
            return `Response validation failed:\n${errors}\n\nReceived:\n${JSON.stringify(parsedResponse, null, 2)}`;
          }

          const toolCallId = config.toolCall?.id;
          if (!toolCallId) {
            throw new Error(
              "Tool call ID is missing in ToolRunnableConfig. Cannot create ToolMessage without a valid tool_call_id.",
            );
          }

          // Get database-related messages for chat history
          const stateMessages =
            (getCurrentTaskInput() as typeof AgentState.State)?.messages ?? [];
          const databaseRelatedMessages = extractToolRelatedMessages(
            stateMessages,
            "databaseRetriever",
          );
          const getSchemaRelatedMessages = extractToolRelatedMessages(
            stateMessages,
            "getSchemasForTables",
          );
          const userQuestion =
            (getCurrentTaskInput() as typeof AgentState.State)?.userQuestion ??
            "";

          // Return Command that sets answer - conditional edge will route to format_response
          return new Command({
            update: {
              answer: validated.data,
              chatHistory: [
                new HumanMessage(userQuestion),
                ...getSchemaRelatedMessages,
                ...databaseRelatedMessages,
                new AIMessage(JSON.stringify(validated.data, null, 2)),
              ],
              messages: [
                new ToolMessage({
                  status: "success",
                  name: "generateResponse",
                  content: JSON.stringify(validated.data, null, 2),
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
        name: "generateResponse",
        description:
          "Generate a final response to the user by executing Python code that prints JSON.\n\n" +
          "**Use this for data-driven responses** (tables, charts). For simple text responses, output JSON directly.\n\n" +
          "**Helper module:** `from inconvo import text, table, chart`\n\n" +
          "**Available libraries:** pandas, numpy, json, altair\n\n" +
          "---\n\n" +
          "## IMPORTANT: File paths\n\n" +
          "Always use the FULL PATH when opening files:\n" +
          "- Database results: `/conversation_data/{sandboxFile}`\n" +
          "- Static datasets: `/datasets/{filename}`\n\n" +
          "✅ CORRECT: `open('/conversation_data/abc-123.json')`\n" +
          "❌ WRONG: `open('abc-123.json')` - File not found!\n\n" +
          "---\n\n" +
          "## IMPORTANT: How to use `chart()`\n\n" +
          "The `chart()` function ONLY accepts **Altair Chart objects**. Do NOT pass dictionaries or raw Vega-Lite specs.\n\n" +
          "✅ CORRECT: `chart(alt.Chart(df).mark_bar().encode(...), 'message')`\n" +
          "❌ WRONG: `chart({'mark': 'bar', ...}, 'message')` - This will fail!\n\n" +
          "The function signature is: `chart(altair_chart_object, message_string)`\n\n" +
          "---\n\n" +
          "## Examples\n\n" +
          "### Table response\n" +
          "```python\n" +
          "from inconvo import table\n" +
          "import pandas as pd\n" +
          "import json\n\n" +
          "# IMPORTANT: Use full path /conversation_data/ + sandboxFile\n" +
          "with open('/conversation_data/{sandboxFile}') as f:\n" +
          "    data = json.load(f)\n" +
          "df = pd.DataFrame(data['data'])\n" +
          "table(df.head(10), 'Top 10 results')\n" +
          "```\n\n" +
          "### Chart response (using Altair)\n" +
          "```python\n" +
          "from inconvo import chart\n" +
          "import altair as alt\n" +
          "import pandas as pd\n" +
          "import json\n\n" +
          "# IMPORTANT: Use full path /conversation_data/ + sandboxFile\n" +
          "with open('/conversation_data/{sandboxFile}') as f:\n" +
          "    data = json.load(f)\n" +
          "df = pd.DataFrame(data['data'])\n\n" +
          "# Create Altair Chart object - pass this to chart()\n" +
          "c = alt.Chart(df).mark_bar().encode(\n" +
          "    x=alt.X('category:N', title='Category'),\n" +
          "    y=alt.Y('value:Q', title='Value')\n" +
          ")\n" +
          "chart(c, 'Sales by category shows X leading at Y%')\n" +
          "```\n\n" +
          "### Text response\n" +
          "```python\n" +
          "from inconvo import text\n" +
          "text('The analysis shows a 15% increase year-over-year.')\n" +
          "```",
        schema: z.object({
          code: z
            .string()
            .describe(
              "Python code using inconvo helpers that prints JSON response to stdout",
            ),
        }),
      },
    );
    tools.push(generateResponse);

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

  // After tools run, check if generateResponse already set the answer
  const shouldContinueAfterTools = (state: typeof AgentState.State) => {
    // If answer is already set (by generateResponse), go directly to format_response
    if (state.answer && state.answer.message) {
      return "formatResponse";
    }
    // Otherwise, go back to the agent for another turn
    return "continue";
  };

  async function callModel(state: typeof AgentState.State) {
    const prompt = await getPrompt("inconvo_agent_gpt5_dev:480f3449");
    const tables = params.schema.map((table) => table.name);

    // Format available datasets for agent context
    const availableDatasets =
      params.availableDatasets && params.availableDatasets.length > 0
        ? params.availableDatasets
            .map((ds) => {
              const schemaInfo = ds.schema
                ? `columns: [${ds.schema.join(", ")}]`
                : "schema: unknown";
              const notesInfo = ds.notes ? ` Notes: "${ds.notes}"` : "";
              return `- ${ds.targetPath}: ${schemaInfo}${notesInfo}`;
            })
            .join("\n")
        : "No datasets available";

    const response = await prompt.pipe(model.bindTools(tools)).invoke({
      tables,
      tableContext: tableContext,
      chatHistory: state.chatHistory,
      date: new Date().toISOString(),
      requestContext: JSON.stringify(state.requestContext),
      userQuestion: new HumanMessage(state.userQuestion),
      messages: state.messages,
      availableDatasets,
    });
    return { messages: [response] };
  }

  async function formatResponse(state: typeof AgentState.State) {
    // If answer is already set (e.g., from generateResponse tool), return it
    // so streaming can detect the completed response
    if (state.answer && state.answer.message) {
      return { answer: state.answer };
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
    .addConditionalEdges("inconvo_agent_tools", shouldContinueAfterTools, {
      continue: "inconvo_agent",
      formatResponse: "format_response",
    })
    .addEdge("format_response", END);

  const graph = workflow.compile({ checkpointer: params.checkpointer });

  return { graph };
}
