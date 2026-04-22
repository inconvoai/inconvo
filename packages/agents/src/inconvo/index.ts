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
import type { SqliteSaver } from "@langchain/langgraph-checkpoint-sqlite";
import type { Schema, DatabaseConnector, QueryResponse } from "@repo/types";
import { getPrompt } from "../utils/getPrompt";
import {
  databaseRetrieverAgent,
  type DatabaseRetrieverError,
} from "../database";
import type { RunnableToolLike } from "@langchain/core/runnables";
import { buildTableSchemaStringFromTableSchema } from "../database/utils/schemaFormatters";
import {
  type DatabaseRetrieverQueryDraft,
  databaseRetrieverQueryDraftSchema,
} from "../database/queryDraft";
import { stringArrayToZodEnum } from "../utils/zodHelpers";
import { getAIModel, type AIProvider } from "../utils/getAIModel";
import { buildPromptCacheKey } from "../utils/promptCacheKey";
import { inconvoResponseSchema } from "@repo/types";
import { tryCatchSync } from "../utils/tryCatch";
import type { Conversation } from "@repo/types";
import { databaseRetrieverToolDescription } from "../database/utils/databaseRetrieverToolDescription";
import { createSandboxClientFromEnv } from "../utils/sandbox";
import { extractTextFromMessage } from "../utils/langchainMessageUtils";

import type { VegaLiteSpec } from "@repo/types";
import type { TopLevelSpec } from "vega-lite";

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

export function formatDatabaseContext(databases: DatabaseConfig[]) {
  return databases
    .map((db) => {
      const descriptionLine = db.context
        ? `  Description: ${db.context}`
        : "  Description: (none)";
      const tableList =
        db.schema.length > 0
          ? db.schema
              .map((table) =>
                table.summary
                  ? `    - ${table.name}: ${table.summary}`
                  : `    - ${table.name}`,
              )
              .join("\n")
          : "    - (none)";
      return `- **${db.friendlyName}**\n${descriptionLine}\n  Tables:\n${tableList}`;
    })
    .join("\n");
}

type SchemaReviewManifestEntry = {
  database: string;
  tables: string[];
};

interface QuestionAgentParams {
  databases: DatabaseConfig[];
  checkpointer: PostgresSaver | MemorySaver | SqliteSaver;
  conversation: Conversation;
  orgId: string;
  agentId: string;
  /** Unique identifier for this run/message. Used to scope the sandbox instance. */
  runId: string;
  /** User identifier for scoping datasets and conversation data. */
  userIdentifier: string;
  /** Available dataset files with metadata for agent context */
  availableDatasets?: AvailableDataset[];
  /** Optional callback to trigger conversation titling (e.g., via Inngest in platform) */
  onTitleConversation?: (
    conversationId: string,
    message: string,
  ) => Promise<void>;
  /** AI provider to use for model calls */
  provider: AIProvider;
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
 * Extracts full assistant tool turns for any assistant message that invoked at least
 * one of the requested tool names. Once an AI message is included, all executed tool
 * outputs from that AI message are included in tool-call order to preserve the
 * request/response pairing required by the OpenAI Responses API.
 */
export function extractToolTurnMessages(
  messages: BaseMessage[],
  toolNames: string[],
) {
  const relatedMessages: BaseMessage[] = [];
  const messageMap = new Map<string, ToolMessage>();

  // First pass: map tool_call_id -> ToolMessage for O(1) lookup
  messages.forEach((msg) => {
    if (ToolMessage.isInstance(msg) && msg.tool_call_id) {
      messageMap.set(msg.tool_call_id, msg);
    }
  });

  // Second pass: find AI messages with at least one relevant tool call and keep
  // every executed tool response for that AI message.
  messages.forEach((msg) => {
    if (AIMessage.isInstance(msg)) {
      const toolCalls = msg.tool_calls ?? [];
      const hasRelevantToolCall = toolCalls.some(
        (call) => !!call.name && toolNames.includes(call.name),
      );

      if (hasRelevantToolCall) {
        relatedMessages.push(msg);
        toolCalls.forEach((call) => {
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

function countToolCalls(messages: BaseMessage[], toolName: string): number {
  return messages.reduce((count, message) => {
    if (!AIMessage.isInstance(message)) {
      return count;
    }
    const toolCalls = [
      ...(message.tool_calls ?? []),
      ...(message.invalid_tool_calls ?? []),
    ];
    return count + toolCalls.filter((call) => call.name === toolName).length;
  }, 0);
}

function mergeSchemaReviewManifest(
  current: SchemaReviewManifestEntry[],
  incoming: SchemaReviewManifestEntry[],
): SchemaReviewManifestEntry[] {
  const manifest = new Map<string, Set<string>>();

  for (const entry of [...current, ...incoming]) {
    if (!manifest.has(entry.database)) {
      manifest.set(entry.database, new Set());
    }

    for (const table of entry.tables) {
      manifest.get(entry.database)!.add(table);
    }
  }

  return Array.from(manifest.entries())
    .sort(([a], [b]) => a.localeCompare(b))
    .map(([database, tables]) => ({
      database,
      tables: Array.from(tables).sort(),
    }));
}

function formatSchemaReviewManifest(
  entries: SchemaReviewManifestEntry[],
): string {
  if (entries.length === 0) {
    return "No previously reviewed schemas in this session.";
  }

  return entries
    .map((entry) => `- ${entry.database}: ${entry.tables.join(", ")}`)
    .join("\n");
}

function createDatabaseRetrieverToolMessage(
  toolCallId: string,
  database: string,
  error: DatabaseRetrieverError,
): ToolMessage {
  return new ToolMessage({
    status: "error",
    name: "databaseRetriever",
    content: JSON.stringify(
      {
        error,
        database,
      },
      null,
      2,
    ),
    tool_call_id: toolCallId,
  });
}

function createDatabaseRetrieverExecutionError(
  message: string,
): DatabaseRetrieverError {
  return {
    type: "execution",
    stage: "execution",
    message,
  };
}

export function findMostRecentDatabaseRetrieverValidationError(
  messages: BaseMessage[],
): DatabaseRetrieverError | null {
  for (let index = messages.length - 1; index >= 0; index -= 1) {
    const message = messages[index];
    if (!message || !ToolMessage.isInstance(message)) {
      continue;
    }

    if (message.name !== "databaseRetriever") {
      continue;
    }

    const content =
      typeof message.content === "string"
        ? message.content
        : message.content
            .map((part) => {
              if (typeof part === "string") {
                return part;
              }
              if ("text" in part && typeof part.text === "string") {
                return part.text;
              }
              return JSON.stringify(part);
            })
            .join("\n");
    if (!content) continue;

    const parsed = tryCatchSync(() => JSON.parse(content) as unknown) as {
      data: unknown;
      error: Error | null;
    };
    if (parsed.error || !parsed.data || typeof parsed.data !== "object") {
      continue;
    }

    const maybeError = (parsed.data as { error?: unknown }).error;
    if (!maybeError || typeof maybeError !== "object") {
      continue;
    }

    const errorRecord = maybeError as Record<string, unknown>;
    if (
      errorRecord.type === "validation" &&
      typeof errorRecord.stage === "string"
    ) {
      return errorRecord as DatabaseRetrieverError;
    }
  }

  return null;
}

export function formatDatabaseRetrieverValidationFeedback(
  error: DatabaseRetrieverError,
  attemptNumber: number,
): string {
  const issues = error.issues?.length
    ? error.issues
        .slice(0, 4)
        .map(
          (issue) => `- ${issue.path} (${issue.code}): ${issue.message}`,
        )
        .join("\n")
    : null;

  return [
    "The most recent `databaseRetriever` call returned a validation error.",
    `Stage: ${error.stage}.`,
    error.message ? `Guidance: ${error.message}` : null,
    issues ? `Issues:\n${issues}` : null,
    `Do not answer the user yet. Repair the structured query draft and retry \`databaseRetriever\`. This was failed attempt ${attemptNumber} of 3.`,
  ]
    .filter(Boolean)
    .join("\n\n");
}

function formatInvalidToolCallFeedback(lastMessage: AIMessage): string {
  const invalidCalls = lastMessage.invalid_tool_calls ?? [];
  const invalidSummary = invalidCalls
    .map((call, index) => {
      const name = call.name ?? `unknown_tool_${index + 1}`;
      const error = call.error ? ` (${call.error})` : "";
      return `- ${name}${error}`;
    })
    .join("\n");

  return [
    "One or more tool calls were malformed and were not executed.",
    invalidSummary ? `Malformed calls:\n${invalidSummary}` : null,
    "Re-emit a valid tool call instead of answering normally.",
    'For `databaseRetriever`, the arguments must be {"database":"...","query":{"table":"...","operation":"...","operationParameters":{...},"questionConditions":null|{"AND":[...]}}}.',
    'For `getSchemasForTables`, the arguments must be {"database":"...","tables":["table_a","table_b"]}.',
  ]
    .filter(Boolean)
    .join("\n\n");
}

/**
 * Validates a Vega-Lite spec by compiling it to Vega and parsing.
 * This catches both structural issues and invalid expression functions.
 * Returns an array of error messages if validation fails.
 *
 * Uses dynamic imports to avoid module-level evaluation that causes
 * RSC serialization errors with Next.js Turbopack.
 * See: https://github.com/vercel/next.js/issues/73323
 */
async function validateVegaLiteSpec(
  response: z.infer<typeof inconvoResponseSchema>,
): Promise<string[]> {
  if (response.type !== "chart") {
    return [];
  }

  // Dynamic imports to avoid module-level Sets causing RSC errors
  const vegaLite = await import("vega-lite");
  const vega = await import("vega");

  // First, try to compile the Vega-Lite spec to a Vega spec.
  // Cast through unknown because our Zod schema is minimal (only validates data.values);
  // full spec validation happens here via vegaLite.compile()
  let compiled;
  try {
    compiled = vegaLite.compile(
      response.spec as unknown as TopLevelSpec,
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
  const conversationUserContext = params.conversation.userContext ?? undefined;

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

          // 4. First pass: identify AI messages to remove and collect ALL their tool call IDs
          const allToolCallIdsToRemove = new Set<string>();
          const aiMessagesToRemove = new Set<BaseMessage>();

          x.forEach((msg) => {
            if (AIMessage.isInstance(msg)) {
              const allCalls = [
                ...(msg.tool_calls ?? []),
                ...(msg.invalid_tool_calls ?? []),
              ];
              const dbCalls = allCalls.filter(
                (c) => c.name === "databaseRetriever" && c.id,
              );
              // If all db calls in this message are in the removal set, remove the message
              if (
                dbCalls.length > 0 &&
                dbCalls.every((c) => c.id && idsToRemove.has(c.id))
              ) {
                aiMessagesToRemove.add(msg);
                // Collect ALL tool call IDs from this message (not just databaseRetriever)
                allCalls.forEach((c) => {
                  if (c.id) allToolCallIdsToRemove.add(c.id);
                });
              }
            }
          });

          // 5. Filter out AI messages and their corresponding ToolMessages
          const filteredHistory = x.filter((msg) => {
            // Remove tool messages whose tool_call_id belongs to a removed AI message
            if (ToolMessage.isInstance(msg) && msg.tool_call_id) {
              if (allToolCallIdsToRemove.has(msg.tool_call_id)) return false;
            }
            // Remove marked AI messages
            if (aiMessagesToRemove.has(msg)) {
              return false;
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
    reviewedSchemas: Annotation<SchemaReviewManifestEntry[]>({
      reducer: (x, y) => mergeSchemaReviewManifest(x, y),
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
      { query?: QueryResponse["query"]; data?: unknown }[] | undefined
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
        query: QueryResponse["query"];
        data: unknown;
        warning?: string;
      }>
    >({
      reducer: (x, y) => (y.length === 0 ? [] : x.concat(y)),
      default: () => [],
    }),
    // Context about the request to pass to the database retriever
    userContext: Annotation<Record<string, string | number | boolean>>({
      reducer: (x, y) => y,
      default: () =>
        (params.conversation.userContext ??
          {}) as Record<string, string | number | boolean>,
    }),
    error: Annotation<Record<string, unknown> | undefined>({
      reducer: (x, y) => y,
      default: () => undefined,
    }),
    lastDatabaseRetrieverValidationRepairAttemptCount: Annotation<number>({
      reducer: (_x, y) => y,
      default: () => 0,
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
    provider: Annotation<AIProvider>({
      reducer: (x, y) => y ?? x,
      default: () => params.provider,
    }),
  });

  const tools: (StructuredToolInterface | DynamicTool | RunnableToolLike)[] =
    [];
  const toolNode = new ToolNode(tools);

  const promptCacheKey = buildPromptCacheKey({
    agentId: params.agentId,
    userIdentifier: params.userIdentifier,
  });

  const model = getAIModel(params.provider, "gpt-5.2", {
    promptCacheKey,
    // reasoning: { effort: "low", summary: "detailed" },
  });

  if (params.databases.length === 0) {
    throw new Error("At least one database is required");
  }

  // Build database names for tool schema validation
  const databaseNames = params.databases.map((db) => db.friendlyName);

  // Build database context for system prompt.
  // Includes database description and per-table semantic summaries.
  const databaseContext = formatDatabaseContext(params.databases);

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
      lastDatabaseRetrieverValidationRepairAttemptCount: 0,
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
        input: { query: DatabaseRetrieverQueryDraft; database: string },
        config: ToolRunnableConfig,
      ) => {
        const toolCallId = config.toolCall?.id;
        if (!toolCallId) {
          throw new Error(
            "Tool call ID is missing in ToolRunnableConfig. Cannot create ToolMessage without a valid tool_call_id.",
          );
        }
        const currentState = getCurrentTaskInput() as typeof AgentState.State;
        const priorAttemptCount = countToolCalls(
          currentState?.messages ?? [],
          "databaseRetriever",
        );
        const attemptNumber = priorAttemptCount + 1;

        try {
          const startedAt = Date.now();

          // Look up the database configuration
          const dbConfig = databaseMap.get(input.database);
          if (!dbConfig) {
            return createDatabaseRetrieverToolMessage(
              toolCallId,
              input.database,
              createDatabaseRetrieverExecutionError(
                `Database "${input.database}" not found. Available databases: ${databaseNames.join(", ")}`,
              ),
            );
          }

          const databaseRetrieverResponse = await (
            await databaseRetrieverAgent({
              query: input.query,
              schema: dbConfig.schema,
              userContext: state.userContext,
              connector: dbConfig.connector,
            })
          ).invoke({});

          console.info(
            JSON.stringify({
              event: "databaseRetriever_attempt",
              component: "inconvo",
              database: input.database,
              database_retriever_attempt_count: attemptNumber,
              question_to_sql_ms:
                databaseRetrieverResponse.preExecutionMs ??
                Date.now() - startedAt,
              database_retriever_total_ms: Date.now() - startedAt,
            }),
          );

          // Check for errors from the database agent
          if (databaseRetrieverResponse.error) {
            return createDatabaseRetrieverToolMessage(
              toolCallId,
              input.database,
              databaseRetrieverResponse.error,
            );
          }

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
          // Fallback for unexpected errors
          return createDatabaseRetrieverToolMessage(
            toolCallId,
            input.database,
            createDatabaseRetrieverExecutionError(
              e instanceof Error ? e.message : String(e),
            ),
          );
        }
      },
      {
        name: "databaseRetriever",
        description: databaseRetrieverToolDescription,
        schema: z.object({
          database: stringArrayToZodEnum(databaseNames).describe(
            "The name of the database to query",
          ),
          query: databaseRetrieverQueryDraftSchema.describe(
            "A complete structured query draft built from the selected table schemas.",
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
          const currentState = getCurrentTaskInput() as typeof AgentState.State;
          const startedAt = Date.now();
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
          console.info(
            JSON.stringify({
              event: "schema_fetch",
              component: "inconvo",
              database: input.database,
              schema_fetch_count: countToolCalls(
                currentState?.messages ?? [],
                "getSchemasForTables",
              ),
              table_count: input.tables.length,
              duration_ms: Date.now() - startedAt,
            }),
          );
          return new Command({
            update: {
              reviewedSchemas: [
                {
                  database: input.database,
                  tables: input.tables,
                },
              ],
              messages: [
                new ToolMessage({
                  status: "success",
                  name: "getSchemasForTables",
                  content: schemaStrings.join("\n\n---\n\n"),
                  tool_call_id: toolCallId,
                }),
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
            userContext: conversationUserContext,
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
            userContext: conversationUserContext,
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
          const toolTurnMessages = extractToolTurnMessages(
            stateMessages,
            ["databaseRetriever", "getCurrentTime"],
          );
          const userQuestion =
            (getCurrentTaskInput() as typeof AgentState.State)?.userQuestion ??
            "";

          // Return Command that sets answer - conditional edge will route to format_response
          return new Command({
            update: {
              sandboxUsed: true,
              answer: validated.data,
              chatHistory: [
                new HumanMessage(userQuestion),
                ...toolTurnMessages.map(sanitizeMessageForHistory),
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
    const hasValidToolCalls =
      AIMessage.isInstance(lastMessage) &&
      Array.isArray(lastMessage.tool_calls) &&
      lastMessage.tool_calls.length > 0;
    const hasInvalidToolCalls =
      AIMessage.isInstance(lastMessage) &&
      Array.isArray(lastMessage.invalid_tool_calls) &&
      lastMessage.invalid_tool_calls.length > 0;
    const databaseRetrieverAttemptCount = countToolCalls(
      messages ?? [],
      "databaseRetriever",
    );
    const mostRecentDatabaseRetrieverValidationError =
      findMostRecentDatabaseRetrieverValidationError(messages ?? []);

    if (hasInvalidToolCalls && !hasValidToolCalls) {
      return "repairInvalidToolCalls";
    }
    if (
      AIMessage.isInstance(lastMessage) &&
      !hasValidToolCalls &&
      mostRecentDatabaseRetrieverValidationError &&
      databaseRetrieverAttemptCount < 3 &&
      databaseRetrieverAttemptCount >
        state.lastDatabaseRetrieverValidationRepairAttemptCount
    ) {
      return "repairDatabaseRetrieverValidation";
    }
    if (
      AIMessage.isInstance(lastMessage) &&
      !hasValidToolCalls
    ) {
      return "formatResponse";
    } else {
      return "continue";
    }
  };

  const repairInvalidToolCalls = async (state: typeof AgentState.State) => {
    const lastMessage = state.messages?.at(-1);
    if (!lastMessage || !AIMessage.isInstance(lastMessage)) {
      return {};
    }

    return {
      messages: [new SystemMessage(formatInvalidToolCallFeedback(lastMessage))],
    };
  };

  const repairDatabaseRetrieverValidation = async (
    state: typeof AgentState.State,
  ) => {
    const attemptCount = countToolCalls(
      state.messages ?? [],
      "databaseRetriever",
    );
    const validationError = findMostRecentDatabaseRetrieverValidationError(
      state.messages ?? [],
    );

    if (!validationError) {
      return {};
    }

    return {
      lastDatabaseRetrieverValidationRepairAttemptCount: attemptCount,
      messages: [
        new SystemMessage(
          formatDatabaseRetrieverValidationFeedback(
            validationError,
            attemptCount,
          ),
        ),
      ],
    };
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
      const allResults: { query: QueryResponse["query"]; data: unknown }[] = [];
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
      userContext: conversationUserContext,
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
    const prompt = await getPrompt("inconvo_agent");

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
      databaseContext: databaseContext,
      chatHistory: state.chatHistory,
      reviewedSchemasManifest: formatSchemaReviewManifest(state.reviewedSchemas),
      date: new Date().toISOString().split("T")[0],
      userContext: JSON.stringify(state.userContext),
      userQuestion: state.userQuestion,
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
          userContext: conversationUserContext,
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
        const vegaErrors = await validateVegaLiteSpec(zodResult.data);
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
    const toolTurnMessages = extractToolTurnMessages(
      stateMessages,
      ["databaseRetriever", "getCurrentTime"],
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
          ...toolTurnMessages.map(sanitizeMessageForHistory),
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
          ...toolTurnMessages.map(sanitizeMessageForHistory),
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
    .addNode("repair_invalid_tool_calls", repairInvalidToolCalls)
    .addNode(
      "repair_database_retriever_validation",
      repairDatabaseRetrieverValidation,
    )
    .addNode("inconvo_agent_tools", toolNode)
    .addNode("process_database_results", processDatabaseResults)
    .addNode("format_response", formatResponse)
    .addEdge(START, "reset_state")
    .addEdge("reset_state", "build_tools")
    .addEdge("reset_state", "title_conversation")
    .addEdge("build_tools", "inconvo_agent")
    .addConditionalEdges("inconvo_agent", shouldContinue, {
      continue: "inconvo_agent_tools",
      repairInvalidToolCalls: "repair_invalid_tool_calls",
      repairDatabaseRetrieverValidation:
        "repair_database_retriever_validation",
      formatResponse: "format_response",
    })
    .addEdge("repair_invalid_tool_calls", "inconvo_agent")
    .addEdge("repair_database_retriever_validation", "inconvo_agent")
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
