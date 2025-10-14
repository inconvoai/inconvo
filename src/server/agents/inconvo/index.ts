import {
  AIMessage,
  HumanMessage,
  type BaseMessage,
  isAIMessage,
  type ToolMessage,
  isToolMessage,
} from "@langchain/core/messages";
import { ToolNode } from "@langchain/langgraph/prebuilt";
import {
  StateGraph,
  type MemorySaver,
  Annotation,
  END,
  START,
} from "@langchain/langgraph";
import {
  type DynamicTool,
  tool,
  type StructuredToolInterface,
} from "@langchain/core/tools";
import { z } from "zod";
import type { PostgresSaver } from "@langchain/langgraph-checkpoint-postgres";
import type { Schema } from "~/server/db/schema";
import { getPrompt } from "../utils/getPrompt";
import { databaseRetrieverAgent } from "../database";
import { Calculator } from "~/server/agents/tools/calculator";
import type { RunnableToolLike } from "@langchain/core/runnables";
import { buildTableSchemaStringFromTableSchema } from "../database/utils/schemaFormatters";
import { stringArrayToZodEnum } from "../utils/zodHelpers";
import { getAIModel } from "../utils/getAIModel";
import { inconvoResponseSchema } from "~/server/userDatabaseConnector/types";
import { tryCatchSync } from "~/server/api/utils/tryCatch";
import { inngest } from "~/server/inngest/client";
import type { Conversation } from "@prisma/client";
import { databaseRetrieverToolDescription } from "../database/utils/databaseRetrieverToolDescription";

interface Chart {
  type: "bar" | "line";
  data: {
    label: string;
    value: number;
  }[];
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

interface QuestionAgentParams {
  organisationName: string;
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

  const model = getAIModel("azure:gpt-5");

  const tableContext = params.schema
    .filter((table) => table.context)
    .map((table) => `Name: ${table.name},\n Context: ${table.context}`)
    .join("\n");

  function resetState(_state: typeof AgentState.State) {
    // We only have to preserve the chat history from the checkpointer
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
      async (input: { question: string }) => {
        try {
          const databaseRetrieverResponse = await (
            await databaseRetrieverAgent({
              userQuestion: input.question,
              organisationName: params.organisationName,
              schema: params.schema,
              requestContext: state.requestContext,
              connectorUrl: params.connectorUrl,
              connectorSigningKey: params.connectorSigningKey,
            })
          ).invoke({});
          return {
            verification: databaseRetrieverResponse.reason,
            query: databaseRetrieverResponse.databaseResponse.query,
            data: databaseRetrieverResponse.databaseResponse.response,
          };
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

    const calculator = new Calculator();
    tools.push(calculator);

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
    const prompt = await getPrompt("inconvo_agent_gpt5_dev:21b08650");
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

  function addDatabaseRetrieverGuidanceMessage(state: typeof AgentState.State) {
    const messages = state.messages ?? [];
    if (messages.length === 0) {
      return {};
    }

    let lastAiMessageIndex = -1;
    for (let i = messages.length - 1; i >= 0; i -= 1) {
      const candidate = messages[i];
      if (!candidate) {
        continue;
      }
      if (isAIMessage(candidate)) {
        lastAiMessageIndex = i;
        break;
      }
    }

    if (lastAiMessageIndex === -1) {
      return {};
    }

    const lastAiMessage = messages[lastAiMessageIndex] as AIMessage;
    const databaseToolCallIds = [
      ...(lastAiMessage.tool_calls ?? []),
      ...(lastAiMessage.invalid_tool_calls ?? []),
    ]
      .filter((call) => call.name === "databaseRetriever" && call.id)
      .map((call) => call.id!);

    if (databaseToolCallIds.length === 0) {
      return {};
    }

    let hasDatabaseRetrieverToolMessage = false;
    for (let i = lastAiMessageIndex + 1; i < messages.length; i += 1) {
      const message = messages[i];
      if (!message) {
        continue;
      }
      if (
        isToolMessage(message) &&
        message.tool_call_id &&
        databaseToolCallIds.includes(message.tool_call_id)
      ) {
        hasDatabaseRetrieverToolMessage = true;
        break;
      }
    }

    if (!hasDatabaseRetrieverToolMessage) {
      return {};
    }

    const guidanceText =
      "You now need to decided if you have enough information to answer the user question.\n" +
      "If you do, answer the question. If you don't, review your available tool calls.;";

    return {
      messages: [new AIMessage(guidanceText)],
    };
  }

  async function formatResponse(state: typeof AgentState.State) {
    // try to zod parse the last message before using the LLM to format it
    // const potentiallyFormattedMessage =
    //   // @ts-ignore-next-line This will be fixed when we use the new langchain package
    //   // with better types for message and message content
    //   state.messages?.at(-1)?.content?.at(-1)?.text ?? "";
    const potentiallyFormattedMessage = state.messages?.at(-1)?.content ?? "";
    const messageContent =
      typeof potentiallyFormattedMessage === "string"
        ? potentiallyFormattedMessage
        : JSON.stringify(potentiallyFormattedMessage);
    const { data: potentiallyFormattedMessageAsJson } = tryCatchSync(
      () => JSON.parse(messageContent) as unknown
    ) as { data: unknown; error: Error | null };

    const stateMessages = state.messages ?? [];
    const databaseRelatedMessages = extractToolRelatedMessages(
      stateMessages,
      "databaseRetriever"
    );
    const getSchemaRelatedMessages = extractToolRelatedMessages(
      stateMessages,
      "getSchemasForTables"
    );

    if (potentiallyFormattedMessageAsJson) {
      const parsedMessage = inconvoResponseSchema.safeParse(
        potentiallyFormattedMessageAsJson
      );
      if (parsedMessage.success) {
        return {
          answer: parsedMessage.data,
          chatHistory: [
            new HumanMessage(state.userQuestion),
            ...getSchemaRelatedMessages,
            ...databaseRelatedMessages,
            new AIMessage(JSON.stringify(parsedMessage.data, null, 2)),
          ],
        };
      } else {
        console.log(
          "Failed to parse message as inconvoResponseSchema",
          parsedMessage.error
        );
      }
    }

    const outputFormatterPrompt = await getPrompt("format_response:84092a5b");

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
        chart: z.object({
          type: z.enum(["line", "bar"]).describe("Chart type"),
          data: z
            .array(
              z.object({
                label: z.string().describe("X axis label"),
                value: z.number().describe("Y axis value"),
              })
            )
            .min(1),
          title: z.string(),
          xLabel: z.string(),
          yLabel: z.string(),
        }),
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

    const outputFormatterSchema = model.withStructuredOutput(wrappedSchema, {
      strict: true,
      method: "jsonSchema",
    });

    const wrapped = await outputFormatterPrompt
      .pipe(outputFormatterSchema)
      .invoke({
        messageToFormat: potentiallyFormattedMessage,
      });

    const response = wrapped.data as z.infer<typeof variantUnion>;

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
    .addNode(
      "post_database_retriever_guidance",
      addDatabaseRetrieverGuidanceMessage
    )
    .addNode("format_response", formatResponse)
    .addEdge(START, "reset_state")
    .addEdge("reset_state", "build_tools")
    .addEdge("reset_state", "title_conversation")
    .addEdge("build_tools", "inconvo_agent")
    .addConditionalEdges("inconvo_agent", shouldContinue, {
      continue: "inconvo_agent_tools",
      formatResponse: "format_response",
    })
    .addEdge("inconvo_agent_tools", "post_database_retriever_guidance")
    .addEdge("post_database_retriever_guidance", "inconvo_agent")
    .addEdge("format_response", END);

  const graph = workflow.compile({ checkpointer: params.checkpointer });

  return { graph };
}
