import { z } from "zod";
import { StateGraph, START, END } from "@langchain/langgraph";
import { MessagesAnnotation } from "@langchain/langgraph";
import { ToolNode } from "@langchain/langgraph/prebuilt";
import { tool } from "@langchain/core/tools";
import {
  questionConditionsSchema,
  type QuestionConditions,
} from "~/server/userDatabaseConnector/types";
import type { Operation } from "../types";
import type {
  TableConditions,
  DateCondition,
} from "~/server/userDatabaseConnector/types";
import type { Schema } from "~/server/db/schema";
import { getAIModel } from "~/server/agents/utils/getAIModel";
import { buildTableSchemaStringFromTableSchema } from "../utils/schemaFormatters";
import { generateJoinGraph } from "../utils/tableRelations";
import { getPrompt } from "../../utils/getPrompt";
import {
  type AIMessage,
  HumanMessage,
  ToolMessage,
  type BaseMessage,
} from "@langchain/core/messages";
import assert from "assert";
import { createQuestionConditionsDynamicSchema } from "./dynamicSchema";
import { aiMessageContainsJsonLikeText } from "../../utils/langchainMessageUtils";

export type WhereConditionArtifact =
  | {
      status: "invalid";
      issues: Array<{
        path: string;
        message: string;
        code: string;
      }>;
      guidance?: string;
    }
  | {
      status: "valid";
      filter: QuestionConditions;
    };

interface RequestParams {
  question: string;
  tableName: string;
  operation: Operation;
  operationParams: Record<string, unknown>;
  schema: Schema;
  tableSchema: Schema[number];
  dateCondition: DateCondition;
  tableConditions: TableConditions;
  requestContext: Record<string, string | number>;
  connectorUrl: string;
  connectorSigningKey: string;
}

export async function questionWhereConditionAgent(params: RequestParams) {
  const llm = getAIModel("azure:gpt-5.1");

  const relatedTables = generateJoinGraph(
    params.schema,
    params.tableName,
    1
  ).uniqueTableNames;

  const relatedTablesSchemasString = relatedTables
    .filter((tableName) => tableName !== params.tableName) // Exclude the starting table
    .map((tableName) => {
      const tableSchema = params.schema.find(
        (table) => table.name === tableName
      );
      if (!tableSchema) {
        throw new Error(`Table ${tableName} not found in schema`);
      }
      return buildTableSchemaStringFromTableSchema(tableSchema);
    })
    .join(",\n");

  const filterValidator = createQuestionConditionsDynamicSchema(
    params.tableSchema,
    params.schema
  );

  const applyFilterTool = tool(
    async (input: {
      candidate: unknown;
    }): Promise<WhereConditionArtifact[]> => {
      const raw = input.candidate;
      const parsed = filterValidator.safeParse(raw);
      if (!parsed.success) {
        const issuesList = parsed.error.issues.map((i) => ({
          path: i.path.join(".") || "<root>",
          message: i.message,
          code: i.code,
        }));
        return [
          {
            status: "invalid" as const,
            issues: issuesList,
            guidance:
              "Revise the candidate so it matches the schema. Only call this tool again with a corrected candidate. When valid, return status 'valid'.",
          },
          {
            status: "invalid" as const,
            issues: issuesList,
          },
        ];
      }
      return [
        { status: "valid" as const, filter: parsed.data },
        { status: "valid" as const, filter: parsed.data },
      ];
    },
    {
      name: "applyFilterTool",
      description:
        "Applies the filter object to the query. Input may be an object, or null. If validation fails you'll receive status=invalid with issues; fix them and try again.",
      schema: z.object({
        candidate: z.union([z.object({}).passthrough(), z.null()]),
      }),
      responseFormat: "content_and_artifact",
    }
  );

  const agentPrompt = await getPrompt("where_condition_agent_5:38907ba5");
  const agentPromptFormatted = (await agentPrompt.invoke({
    tableName: params.tableName,
    operation: params.operation,
    date: new Date().toISOString(),
    operationParams: JSON.stringify(params.operationParams, null, 2),
    tableConditions: JSON.stringify(params.tableConditions, null, 2),
    tableSchema: buildTableSchemaStringFromTableSchema(params.tableSchema),
    relatedTablesSchemas: relatedTablesSchemasString,
  })) as Record<"messages", BaseMessage[]>;

  const modelWithTools = llm.bindTools([applyFilterTool]);
  type MsgState = typeof MessagesAnnotation.State;

  const callModel = async (state: MsgState) => {
    const response = await modelWithTools.invoke(state.messages);
    return { messages: [response] };
  };

  const jsonDetectedFeedback = async (_state: MsgState) => {
    return {
      messages: new HumanMessage(
        "JSON-like content was detected in your last response, but you did not call applyFilterTool. \n" +
          "You MUST now call applyFilterTool exactly once with a candidate filter object to apply the filter. \n" +
          "Call the tool with { candidate: <object-or-null> }."
      ),
    };
  };

  const shouldContinue = (state: MsgState) => {
    const last = state.messages.at(-1) as AIMessage;
    // If model requested tool calls -> go to tools node
    if (last && Array.isArray(last.tool_calls) && last.tool_calls.length > 0) {
      return "message_filter_agent_tools";
    }
    if (aiMessageContainsJsonLikeText(last)) {
      return "json_detected_feedback";
    }
    return END;
  };

  const hasValidWhereCondition = (state: MsgState) => {
    const last = state.messages.at(-1) as ToolMessage;
    if (last.name === "applyFilterTool") {
      const whereConditionArtifact = last.artifact as WhereConditionArtifact;
      if (whereConditionArtifact.status === "valid") {
        return END;
      }
    }
    return "message_filter_agent";
  };

  const workflow = new StateGraph(MessagesAnnotation)
    .addNode("message_filter_agent", callModel)
    .addNode("json_detected_feedback", jsonDetectedFeedback)
    .addNode("message_filter_agent_tools", new ToolNode([applyFilterTool]))
    .addEdge(START, "message_filter_agent")
    .addConditionalEdges("message_filter_agent", shouldContinue, {
      message_filter_agent_tools: "message_filter_agent_tools",
      json_detected_feedback: "json_detected_feedback",
      [END]: END,
    })
    .addEdge("json_detected_feedback", "message_filter_agent")
    .addConditionalEdges("message_filter_agent_tools", hasValidWhereCondition, {
      [END]: END,
      message_filter_agent: "message_filter_agent",
    });

  const app = workflow.compile();

  const firstPromptMessage = agentPromptFormatted.messages[0];
  assert(firstPromptMessage, "Prompt did not return an initial message");
  const questionMessage = new HumanMessage(params.question);
  const initialMessages = [firstPromptMessage, questionMessage];

  const graphResult = await app.invoke({ messages: initialMessages });

  const validToolMessage = graphResult.messages.toReversed().find((m) => {
    if (!ToolMessage.isInstance(m)) return false;
    if (m.name !== "applyFilterTool") return false;
    const art = m.artifact as WhereConditionArtifact;
    return art.status === "valid" && art.filter !== undefined;
  }) as ToolMessage | undefined;

  const artifact = validToolMessage?.artifact as
    | WhereConditionArtifact
    | undefined;
  const parsedQuestionConditions = questionConditionsSchema.parse(
    artifact?.status === "valid" ? artifact.filter : null
  );
  return parsedQuestionConditions;
}
