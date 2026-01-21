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

interface DatabaseConfig {
  friendlyName: string;
  context: string | null;
  schema: Schema;
  connector: DatabaseConnector;
}

interface QuestionAgentParams {
  databases: DatabaseConfig[];
  checkpointer: PostgresSaver | MemorySaver;
  conversation: Conversation;
  orgId: string;
  agentId: string;
  /** Unique identifier for this run/message. Used to scope the sandbox instance. */
  runId: string;
  /** User identifier for scoping datasets and conversation data. */
  userIdentifier: string;
  /** Available dataset files with metadata for agent context */
  availableDatasets?: AvailableDataset[];
  /** User context for context-scoped dataset mounting in sandbox. */
  userContext?: Record<string, string | number>;
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
 * Strips metadata from messages to ensure consistent serialization for prompt caching.
 * OpenAI prompt caching requires identical byte-level prefixes, so we remove
 * response_metadata, additional_kwargs, tool_call_chunks, etc. that vary between requests.
 */
function sanitizeMessageForHistory(msg: BaseMessage): BaseMessage {
  if (HumanMessage.isInstance(msg)) {
    return new HumanMessage(msg.content);
  }
  if (AIMessage.isInstance(msg)) {
    // Preserve tool_calls as they're needed for conversation context
    return new AIMessage({
      content: msg.content,
      tool_calls: msg.tool_calls,
    });
  }
  if (ToolMessage.isInstance(msg)) {
    return new ToolMessage({
      content: msg.content,
      tool_call_id: msg.tool_call_id,
      name: msg.name,
    });
  }
  // For any other message type, return as-is
  return msg;
}

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
  // Cast through unknown because our Zod schema is minimal (only validates data.values);
  // full spec validation happens here via vegaLite.compile()
  let compiled;
  try {
    compiled = vegaLite.compile(
      response.spec as unknown as vegaLite.TopLevelSpec,
    );
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
        const MAX_DB_RETRIEVER_CALLS_TO_KEEP = 5;

        // When new messages contain database retriever calls,
        // we keep only the last N databaseRetriever call pairs
        if (hasDatabaseRetrieverCall(y)) {
          // 1. Count how many new db retriever calls are coming in
          let newDbCallCount = 0;
          y.forEach((msg) => {
            if (AIMessage.isInstance(msg)) {
              const calls = [
                ...(msg.tool_calls ?? []),
                ...(msg.invalid_tool_calls ?? []),
              ];
              calls.forEach((call) => {
                if (call.name === "databaseRetriever") {
                  newDbCallCount++;
                }
              });
            }
          });

          // 2. Collect all databaseRetriever tool_call ids from history in order
          const dbToolCallIdsInOrder: string[] = [];
          x.forEach((msg) => {
            if (AIMessage.isInstance(msg)) {
              const calls = [
                ...(msg.tool_calls ?? []),
                ...(msg.invalid_tool_calls ?? []),
              ];
              calls.forEach((call) => {
                if (call.name === "databaseRetriever" && call.id) {
                  dbToolCallIdsInOrder.push(call.id);
                }
              });
            }
          });

          // 3. Determine which tool call IDs to remove
          // Keep enough from history so total (history + new) <= MAX
          const historyToKeep = Math.max(
            0,
            MAX_DB_RETRIEVER_CALLS_TO_KEEP - newDbCallCount,
          );
          const idsToRemove = new Set(
            dbToolCallIdsInOrder.slice(
              0,
              Math.max(0, dbToolCallIdsInOrder.length - historyToKeep),
            ),
          );

          // 4. Filter out old databaseRetriever messages, keeping the last N
          const filteredHistory = x.filter((msg) => {
            // Remove only tool messages whose tool_call_id is in the removal set
            if (ToolMessage.isInstance(msg) && msg.tool_call_id) {
              if (idsToRemove.has(msg.tool_call_id)) return false;
            }
            // Remove AI messages that contain databaseRetriever tool calls to be removed
            if (AIMessage.isInstance(msg)) {
              const dbCalls = [
                ...(msg.tool_calls ?? []),
                ...(msg.invalid_tool_calls ?? []),
              ].filter((c) => c.name === "databaseRetriever" && c.id);
              // If all db calls in this message are in the removal set, remove the message
              if (
                dbCalls.length > 0 &&
                dbCalls.every((c) => c.id && idsToRemove.has(c.id))
              ) {
                return false;
              }
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
    // Pending database responses to be processed by process_database_results node
    // Supports parallel databaseRetriever calls - empty array clears the state
    pendingDatabaseResponse: Annotation<
      Array<{
        toolCallId: string;
        database: string;
        query: string;
        data: unknown;
        warning?: string;
      }>
    >({
      reducer: (x, y) => (y.length === 0 ? [] : x.concat(y)),
      default: () => [],
    }),
    // Context about the request to pass to the database retriever
    userContext: Annotation<Record<string, string | number>>({
      reducer: (x, y) => y,
      default: () =>
        params.conversation.userContext as Record<string, string | number>,
    }),
    error: Annotation<Record<string, unknown> | undefined>({
      reducer: (x, y) => y,
      default: () => undefined,
    }),
    formatAttempts: Annotation<number>({
      reducer: (x, y) => x + y,
      default: () => 0,
    }),
    // Track if sandbox was used this run (to avoid unnecessary destroy calls)
    sandboxUsed: Annotation<boolean>({
      reducer: (x, y) => y,
      default: () => false,
    }),
  });

  const tools: (StructuredToolInterface | DynamicTool | RunnableToolLike)[] =
    [];
  const toolNode = new ToolNode(tools);

  const promptCacheKey = buildPromptCacheKey({
    agentId: params.agentId,
    userContext: params.conversation.userContext as Record<
      string,
      string | number
    >,
  });

  const model = getAIModel("azure:gpt-5.2", {
    promptCacheKey,
    // reasoning: { effort: "low", summary: "detailed" },
  });

  // Build database names for tool schema validation
  const databaseNames = params.databases.map((db) => db.friendlyName);

  // Build combined table context from all databases
  const tableContext = params.databases
    .flatMap((db) =>
      db.schema
        .filter((table) => table.context)
        .map(
          (table) =>
            `Database: ${db.friendlyName}, Table: ${table.name}, Context: ${table.context}`,
        ),
    )
    .join("\n");

  // Build database context for system prompt
  const databaseContext = params.databases
    .map((db) => {
      const tableNames = db.schema.map((t) => t.name).join(", ");
      const contextInfo = db.context ? ` - ${db.context}` : "";
      return `- **${db.friendlyName}**${contextInfo}\n  Tables: ${tableNames}`;
    })
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
      runId: params.runId, // Use runId from params to scope sandbox instance
      sandboxUsed: false, // Reset sandbox tracking for new run
    };
  }

  function buildTools(state: typeof AgentState.State) {
    // Build a map for quick database lookup
    const databaseMap = new Map(
      params.databases.map((db) => [db.friendlyName, db]),
    );

    const databaseRetriever = tool(
      async (
        input: { question: string; database: string },
        config: ToolRunnableConfig,
      ) => {
        const toolCallId = config.toolCall?.id;
        if (!toolCallId) {
          throw new Error(
            "Tool call ID is missing in ToolRunnableConfig. Cannot create ToolMessage without a valid tool_call_id.",
          );
        }

        try {
          // Look up the database configuration
          const dbConfig = databaseMap.get(input.database);
          if (!dbConfig) {
            return new ToolMessage({
              content: `Tool error: Please check your input and try again. (Database "${input.database}" not found. Available databases: ${databaseNames.join(", ")})`,
              tool_call_id: toolCallId,
            });
          }

          const databaseRetrieverResponse = await (
            await databaseRetrieverAgent({
              userQuestion: input.question,
              schema: dbConfig.schema,
              userContext: state.userContext,
              agentId: params.agentId,
              connector: dbConfig.connector,
            })
          ).invoke({});

          // Store raw response in state for process_database_results node
          return new Command({
            update: {
              pendingDatabaseResponse: [
                {
                  toolCallId,
                  database: input.database,
                  query: databaseRetrieverResponse.databaseResponse.query,
                  data: databaseRetrieverResponse.databaseResponse.response,
                  warning: databaseRetrieverResponse.databaseResponse.warning,
                },
              ],
            },
          });
        } catch (e) {
          return new ToolMessage({
            content: `Tool error: Please check your input and try again. (${e instanceof Error ? e.message : String(e)})`,
            tool_call_id: toolCallId,
          });
        }
      },
      {
        name: "databaseRetriever",
        description: databaseRetrieverToolDescription,
        schema: z.object({
          question: z.string().describe("The question to ask the database"),
          database: stringArrayToZodEnum(databaseNames).describe(
            "The name of the database to query",
          ),
        }),
      },
    );
    tools.push(databaseRetriever);

    const getSchemasForTables = tool(
      async (
        input: { tables: string[]; database: string },
        config: ToolRunnableConfig,
      ) => {
        const toolCallId = config.toolCall?.id;
        if (!toolCallId) {
          throw new Error(
            "Tool call ID is missing in ToolRunnableConfig. Cannot create ToolMessage without a valid tool_call_id.",
          );
        }

        try {
          const dbConfig = databaseMap.get(input.database);
          if (!dbConfig) {
            return new ToolMessage({
              content: `Tool error: Please check your input and try again. (Database "${input.database}" not found. Available databases: ${databaseNames.join(", ")})`,
              tool_call_id: toolCallId,
            });
          }

          const schemaStrings = input.tables.map((tableName) => {
            const table = dbConfig.schema.find((s) => s.name === tableName);
            if (!table) {
              throw new Error(
                `Table "${tableName}" not found in database "${input.database}"`,
              );
            }
            return buildTableSchemaStringFromTableSchema(table);
          });
          return schemaStrings.join("\n\n---\n\n");
        } catch (e) {
          return new ToolMessage({
            content: `Tool error: Please check your input and try again. (${e instanceof Error ? e.message : String(e)})`,
            tool_call_id: toolCallId,
          });
        }
      },
      {
        name: "getSchemasForTables",
        description:
          "Get the schemas for a list of tables (one or more) from a specific database.",
        schema: z.object({
          database: stringArrayToZodEnum(databaseNames).describe(
            "The name of the database containing the tables",
          ),
          tables: z.array(z.string().describe("The name of the table")),
        }),
      },
    );
    tools.push(getSchemasForTables);

    const getCurrentTime = tool(
      async () => {
        return new Date().toISOString();
      },
      {
        name: "getCurrentTime",
        description:
          "Get the current date and time. Use this when you need the precise current time for time-sensitive queries or calculations.",
        schema: z.object({}),
      },
    );
    tools.push(getCurrentTime);

    const executePythonCode = tool(
      async (input: { code: string }, config: ToolRunnableConfig) => {
        try {
          const currentState = getCurrentTaskInput() as typeof AgentState.State;
          const sandboxClient = createSandboxClientFromEnv({
            orgId: params.orgId,
            agentId: params.agentId,
          });
          const sandboxSession = sandboxClient.sandbox({
            conversationId: params.conversation.id,
            runId: currentState?.runId ?? "",
            userIdentifier: params.userIdentifier,
            userContext: params.userContext,
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
          "Executes Python 3 code in a sandboxed environment for intermediate calculations and data analysis.\n\n" +
          "**Use this for intermediate analysis.** For final responses to the user, use `generateResponse` instead.\n\n" +
          "**File paths (IMPORTANT - always use full paths):**\n" +
          "- Database results: `/conversation_data/{sandboxFile}`\n" +
          "- Datasets: Use the exact `targetPath` from the available datasets list\n\n" +
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
          const currentState = getCurrentTaskInput() as typeof AgentState.State;
          const sandboxClient = createSandboxClientFromEnv({
            orgId: params.orgId,
            agentId: params.agentId,
          });
          const sandboxSession = sandboxClient.sandbox({
            conversationId: params.conversation.id,
            runId: currentState?.runId ?? "",
            userIdentifier: params.userIdentifier,
            userContext: params.userContext,
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

          // Get tool-related messages for chat history
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
          const getCurrentTimeRelatedMessages = extractToolRelatedMessages(
            stateMessages,
            "getCurrentTime",
          );
          const userQuestion =
            (getCurrentTaskInput() as typeof AgentState.State)?.userQuestion ??
            "";

          // Combine and deduplicate messages (AI messages may appear in multiple extractions)
          const allToolMessages = [
            ...getSchemaRelatedMessages,
            ...getCurrentTimeRelatedMessages,
            ...databaseRelatedMessages,
          ];
          const uniqueToolMessages = allToolMessages.filter(
            (msg, index, self) => self.indexOf(msg) === index,
          );

          // Return Command that sets answer - conditional edge will route to format_response
          return new Command({
            update: {
              sandboxUsed: true,
              answer: validated.data,
              chatHistory: [
                new HumanMessage(userQuestion),
                ...uniqueToolMessages.map(sanitizeMessageForHistory),
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
          "- Datasets: Use the exact `targetPath` from the available datasets list\n\n" +
          "✅ CORRECT: `open('/conversation_data/abc-123.json')`\n" +
          "❌ WRONG: `open('abc-123.json')` - File not found!\n\n" +
          "---\n\n" +
          "## IMPORTANT: How to use `chart()`\n\n" +
          "The `chart()` function ONLY accepts **Altair Chart objects**. Do NOT pass dictionaries or raw Vega-Lite specs.\n\n" +
          "✅ CORRECT: `chart(alt.Chart(df).mark_bar().encode(...), 'message')`\n" +
          "❌ WRONG: `chart({'mark': 'bar', ...}, 'message')` - This will fail!\n\n" +
          "The function signature is: `chart(altair_chart_object, message_string)`\n\n" +
          "**For complex charts:** Take time to carefully plan your approach. Consider the data structure, appropriate chart type, axis encodings, color schemes, and any transformations needed. Think through edge cases like missing data or unexpected values.\n\n" +
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

  // After tools run, check if we need to process database results or if answer is set
  const routeAfterTools = (state: typeof AgentState.State) => {
    // Check for pending database results first
    if (state.pendingDatabaseResponse.length > 0) {
      return "processDatabaseResults";
    }
    // If answer is already set (by generateResponse), go directly to format_response
    if (state.answer && state.answer.message) {
      return "formatResponse";
    }
    // Otherwise, go back to the agent for another turn
    return "continue";
  };

  // Process database results: upload to S3, truncate for LLM, build ToolMessages
  async function processDatabaseResults(state: typeof AgentState.State) {
    const pendingItems = state.pendingDatabaseResponse;
    if (pendingItems.length === 0) {
      return {};
    }

    const sandboxClient = createSandboxClientFromEnv({
      orgId: params.orgId,
      agentId: params.agentId,
    });

    const DATA_PREVIEW_LIMIT = 50;
    const allResults: { query: string; data: unknown }[] = [];
    const allMessages: ToolMessage[] = [];

    // Process all pending database results (supports parallel tool calls)
    for (const pending of pendingItems) {
      // Upload full data to sandbox S3 storage
      const resultData = { query: pending.query, data: pending.data };
      const sandboxFileName = `${uuidv4()}.json`;
      const jsonString = JSON.stringify(resultData);
      const base64Content = Buffer.from(jsonString, "utf-8").toString("base64");
      await sandboxClient.uploadConversationData({
        conversationId: params.conversation.id,
        userIdentifier: params.userIdentifier,
        files: [
          {
            name: sandboxFileName,
            content: base64Content,
            contentType: "application/json",
          },
        ],
      });

      // Truncate data for LLM response (50 rows max)
      const fullData = pending.data;
      const isArray = Array.isArray(fullData);
      const totalRows = isArray ? fullData.length : 1;
      const wasTruncated = isArray && totalRows > DATA_PREVIEW_LIMIT;
      const truncatedData = isArray
        ? (fullData as unknown[]).slice(0, DATA_PREVIEW_LIMIT)
        : fullData;

      // Build note if data was truncated or hit 1000-row DB limit
      const hit1000Limit = !!pending.warning;
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

      // Build LLM response with truncated data
      const llmResponse: Record<string, unknown> = {
        query: pending.query,
        data: truncatedData,
        sandboxFile: sandboxFileName,
      };
      if (note) {
        llmResponse.note = note;
      }

      allResults.push(resultData);
      allMessages.push(
        new ToolMessage({
          status: "success",
          name: "databaseRetriever",
          content: JSON.stringify(llmResponse),
          tool_call_id: pending.toolCallId,
        }),
      );
    }

    // Pre-warm sandbox in background for later code execution
    // This must happen AFTER uploads complete to avoid s3fs caching stale directory listings
    const sandboxSession = sandboxClient.sandbox({
      conversationId: params.conversation.id,
      runId: state.runId,
      userIdentifier: params.userIdentifier,
      userContext: params.userContext,
    });
    void sandboxSession.start();

    return {
      sandboxUsed: true, // Sandbox was pre-warmed and data uploaded
      pendingDatabaseResponse: [], // Clear the array
      databaseRetrieverResults: allResults,
      messages: allMessages,
    };
  }

  async function callModel(state: typeof AgentState.State) {
    const prompt = await getPrompt("inconvo_agent_gpt5_dev:c092a503");

    // Format tables as a markdown list grouped by database
    const tables = params.databases
      .map((db) => {
        const tableList = db.schema
          .map((table) => `  - ${table.name}`)
          .join("\n");
        return `- ${db.friendlyName}\n${tableList}`;
      })
      .join("\n");

    // Format available datasets for agent context
    const availableDatasets = (() => {
      if (!params.availableDatasets || params.availableDatasets.length === 0) {
        return "No datasets available";
      }

      // Group datasets by scope (user vs context)
      const userDatasets: AvailableDataset[] = [];
      const contextDatasets: Map<string, AvailableDataset[]> = new Map();

      for (const ds of params.availableDatasets) {
        // Context datasets have paths like /datasets/context_{key}/filename
        const contextMatch = ds.targetPath.match(
          /\/datasets\/context_([^/]+)\//,
        );
        if (contextMatch?.[1]) {
          const contextKey = contextMatch[1];
          if (!contextDatasets.has(contextKey)) {
            contextDatasets.set(contextKey, []);
          }
          contextDatasets.get(contextKey)!.push(ds);
        } else {
          userDatasets.push(ds);
        }
      }

      const formatDataset = (ds: AvailableDataset) => {
        const lines = [`  - ${ds.name} (${ds.targetPath})`];
        if (ds.schema && ds.schema.length > 0) {
          lines.push(`    Columns: ${ds.schema.join(", ")}`);
        }
        if (ds.notes) {
          lines.push(`    Notes: ${ds.notes}`);
        }
        return lines.join("\n");
      };

      const sections: string[] = [];

      // User-specific datasets
      if (userDatasets.length > 0) {
        sections.push(
          `User-specific datasets:\n${userDatasets.map(formatDataset).join("\n")}`,
        );
      }

      // Context-scoped datasets (shared)
      for (const [contextKey, datasets] of contextDatasets) {
        sections.push(
          `Shared datasets (${contextKey}):\n${datasets.map(formatDataset).join("\n")}`,
        );
      }

      return sections.join("\n\n");
    })();

    const response = await prompt.pipe(model.bindTools(tools)).invoke({
      tables,
      tableContext: tableContext,
      databaseContext: databaseContext,
      chatHistory: state.chatHistory,
      date: new Date().toISOString().split("T")[0],
      userContext: JSON.stringify(state.userContext),
      userQuestion: new HumanMessage(state.userQuestion),
      messages: state.messages,
      availableDatasets,
    });
    return { messages: [response] };
  }

  async function formatResponse(state: typeof AgentState.State) {
    // Helper to destroy sandbox at end of run (message-scoped cleanup)
    // Only destroys if sandbox was actually used this run
    const destroySandbox = async () => {
      if (!state.sandboxUsed) {
        return; // Skip destroy if sandbox was never used
      }
      try {
        const sandboxClient = createSandboxClientFromEnv({
          orgId: params.orgId,
          agentId: params.agentId,
        });
        const sandboxSession = sandboxClient.sandbox({
          conversationId: params.conversation.id,
          runId: state.runId,
          userIdentifier: params.userIdentifier,
          userContext: params.userContext,
        });
        await sandboxSession.destroy();
      } catch {
        // Ignore errors - sandbox may not have been created
      }
    };

    // If answer is already set (e.g., from generateResponse tool), return it
    // so streaming can detect the completed response
    if (state.answer && state.answer.message) {
      void destroySandbox();
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
    const getCurrentTimeRelatedMessages = extractToolRelatedMessages(
      stateMessages,
      "getCurrentTime",
    );

    // Combine and deduplicate messages (AI messages may appear in multiple extractions)
    const allToolMessages = [
      ...getSchemaRelatedMessages,
      ...getCurrentTimeRelatedMessages,
      ...databaseRelatedMessages,
    ];
    const uniqueToolMessages = allToolMessages.filter(
      (msg, index, self) => self.indexOf(msg) === index,
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

      void destroySandbox();
      return {
        answer: selectedResponse,
        chatHistory: [
          new HumanMessage(state.userQuestion),
          ...uniqueToolMessages.map(sanitizeMessageForHistory),
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

      void destroySandbox();
      return {
        answer: {
          type: "text" as const,
          message: fallbackMessage,
        },
        chatHistory: [
          new HumanMessage(state.userQuestion),
          ...uniqueToolMessages.map(sanitizeMessageForHistory),
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
    .addNode("process_database_results", processDatabaseResults)
    .addNode("format_response", formatResponse)
    .addEdge(START, "reset_state")
    .addEdge("reset_state", "build_tools")
    .addEdge("reset_state", "title_conversation")
    .addEdge("build_tools", "inconvo_agent")
    .addConditionalEdges("inconvo_agent", shouldContinue, {
      continue: "inconvo_agent_tools",
      formatResponse: "format_response",
    })
    .addConditionalEdges("inconvo_agent_tools", routeAfterTools, {
      continue: "inconvo_agent",
      formatResponse: "format_response",
      processDatabaseResults: "process_database_results",
    })
    .addEdge("process_database_results", "inconvo_agent")
    .addEdge("format_response", END);

  const graph = workflow.compile({ checkpointer: params.checkpointer });

  return { graph };
}
