import {
  HumanMessage,
  ToolMessage,
  type AIMessage,
  type BaseMessage,
} from "@langchain/core/messages";
import type { StructuredTool } from "@langchain/core/tools";
import { ToolNode } from "@langchain/langgraph/prebuilt";
import {
  END,
  MessagesAnnotation,
  START,
  StateGraph,
} from "@langchain/langgraph";
import type { Schema } from "@repo/types";
import { getAIModel } from "../../../utils/getAIModel";
import { getPrompt } from "../../../utils/getPrompt";
import { aiMessageContainsJsonLikeText } from "../../../utils/langchainMessageUtils";
import { buildTableSchemaStringFromTableSchema } from "../../utils/schemaFormatters";
import { operationDocs } from "../../utils/operationDocs";

interface CreateOperationParametersAgentOptions<Result, Artifact> {
  tool: StructuredTool;
  toolName: string;
  promptCacheKey: string;
  isValidArtifact?: (artifact: Artifact | undefined) => boolean;
  onComplete: (artifact: Artifact | undefined) => Result | Promise<Result>;
  jsonDetectedMessage?: string;
}

interface OperationParametersAgent<Result> {
  invoke: (args: { messages: BaseMessage[] }) => Promise<Result>;
}

export function createOperationParametersAgent<Result, Artifact>(
  options: CreateOperationParametersAgentOptions<Result, Artifact>,
): OperationParametersAgent<Result> {
  const isValidArtifact =
    options.isValidArtifact ??
    ((artifact?: Artifact) => {
      if (!artifact || typeof artifact !== "object") return false;
      if (!("status" in artifact)) return false;
      const status = (artifact as { status?: unknown }).status;
      return status === "valid";
    });

  const modelWithTools = getAIModel("azure:gpt-5.2", {
    // reasoning: { effort: "low", summary: "detailed" },
    promptCacheKey: options.promptCacheKey,
  }).bindTools([options.tool]);
  type MsgState = typeof MessagesAnnotation.State;

  const callModel = async (state: MsgState) => {
    const response = await modelWithTools.invoke(state.messages);
    return { messages: [response] };
  };

  const jsonDetectedFeedback = async (_state: MsgState) => {
    return {
      messages: new HumanMessage(
        options.jsonDetectedMessage ??
          `You produced JSON-like content but did not call ${options.toolName}. You MUST now call it exactly once with { candidateOperationParameters: <object> }.`,
      ),
    };
  };

  const shouldContinue = (state: MsgState) => {
    const last = state.messages.at(-1) as AIMessage;
    if (last && Array.isArray(last.tool_calls) && last.tool_calls.length > 0) {
      return "message_operation_params_agent_tools";
    }
    if (aiMessageContainsJsonLikeText(last)) {
      return "json_detected_feedback";
    }
    return END;
  };

  const hasValidOperationParams = (state: MsgState) => {
    const last = state.messages.at(-1);
    if (
      !last ||
      !ToolMessage.isInstance(last) ||
      last.name !== options.toolName
    ) {
      return "message_operation_params_agent";
    }
    const artifact = (last as ToolMessage & { artifact?: Artifact })
      .artifact as Artifact | undefined;
    if (isValidArtifact(artifact)) {
      return END;
    }
    return "message_operation_params_agent";
  };

  const workflow = new StateGraph(MessagesAnnotation)
    .addNode("message_operation_params_agent", callModel)
    .addNode("json_detected_feedback", jsonDetectedFeedback)
    .addNode(
      "message_operation_params_agent_tools",
      new ToolNode([options.tool]),
    )
    .addEdge(START, "message_operation_params_agent")
    .addConditionalEdges("message_operation_params_agent", shouldContinue, {
      message_operation_params_agent_tools:
        "message_operation_params_agent_tools",
      json_detected_feedback: "json_detected_feedback",
      [END]: END,
    })
    .addEdge("json_detected_feedback", "message_operation_params_agent")
    .addConditionalEdges(
      "message_operation_params_agent_tools",
      hasValidOperationParams,
      {
        [END]: END,
        message_operation_params_agent: "message_operation_params_agent",
      },
    );

  const app = workflow.compile();

  return {
    async invoke(args: { messages: BaseMessage[] }) {
      const graphResult = await app.invoke({ messages: args.messages });
      const toolMessage = graphResult.messages.toReversed().find((message) => {
        if (!ToolMessage.isInstance(message)) return false;
        return message.name === options.toolName;
      }) as (ToolMessage & { artifact?: Artifact }) | undefined;

      const artifact = toolMessage?.artifact as Artifact | undefined;
      return options.onComplete(artifact);
    },
  };
}

function formatCurrentDate(): string {
  const now = new Date();
  const day = now.getDate();
  const months = [
    "Jan",
    "Feb",
    "Mar",
    "Apr",
    "May",
    "Jun",
    "Jul",
    "Aug",
    "Sep",
    "Oct",
    "Nov",
    "Dec",
  ];
  const month = months[now.getMonth()];
  const year = now.getFullYear();
  return `${day} ${month} ${year}`;
}

export async function buildOperationParametersPromptMessages(
  params: {
    operation: keyof typeof operationDocs;
    tableName: string;
    question: string;
    userContext: Record<string, string | number>;
  },
  tableSchema: string,
): Promise<BaseMessage[]> {
  const prompt = await getPrompt("extend_query:6e6c8f25");
  const formatted = (await prompt.invoke({
    operation: params.operation,
    table: params.tableName,
    operationDocs: JSON.stringify(operationDocs[params.operation], null, 2),
    tableSchema,
    question: params.question,
    userContext: JSON.stringify(params.userContext, null, 2),
    currentDate: formatCurrentDate(),
  })) as Record<"messages", BaseMessage[]>;
  return formatted.messages;
}

export function buildOperationParametersTableSchemasString(
  schema: Schema,
  tableNames: string[],
): string {
  return tableNames
    .map((tableName) => {
      const schemaEntry = schema.find(
        (table: Schema[number]) => table.name === tableName,
      );
      return schemaEntry
        ? buildTableSchemaStringFromTableSchema(schemaEntry)
        : `# Missing schema for ${tableName}`;
    })
    .join("\n---\n");
}
