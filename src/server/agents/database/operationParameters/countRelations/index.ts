import assert from "assert";
import { z } from "zod";
import type {
  BaseMessage,
  ToolMessage,
  AIMessage,
} from "@langchain/core/messages";
import { HumanMessage, isToolMessage } from "@langchain/core/messages";
import { tool } from "@langchain/core/tools";
import { ToolNode } from "@langchain/langgraph/prebuilt";
import {
  MessagesAnnotation,
  START,
  END,
  StateGraph,
} from "@langchain/langgraph";
import { getAIModel } from "~/server/agents/utils/getAIModel";
import { getPrompt } from "~/server/agents/utils/getPrompt";
import { buildTableSchemaStringFromTableSchema } from "../../utils/schemaFormatters";
import { operationDocs } from "../../utils/operationDocs";
import type { Schema } from "~/server/db/schema";
import type { CountRelationsQuery } from "~/server/userDatabaseConnector/types";
import {
  validateCountRelationsCandidate,
  buildCountRelationsZodSchema,
} from "./countRelationsValidator";
import type {
  CountRelationsValidatorContext,
  CountRelationsValidationResult,
} from "./countRelationsValidator";

// Extended ToolMessage type with artifact
interface ToolMessageWithArtifact extends ToolMessage {
  artifact?: CountRelationsValidationResult;
}

export interface DefineCountRelationsOperationParametersParams {
  schema: Schema;
  tableName: string;
  question: string;
  tableSchema: Schema[number];
  operation: "countRelations";
}

export async function defineCountRelationsOperationParameters(
  params: DefineCountRelationsOperationParametersParams
) {
  const model = getAIModel("azure:gpt-5");

  // Gather base columns
  const baseTable = params.schema.find((t) => t.name === params.tableName);
  assert(baseTable, "Base table not found");
  const baseColumns = baseTable.columns.map((c) => c.name);

  // Outward list relations only
  const outwardListRelations = params.tableSchema.outwardRelations.filter(
    (r) => r.isList
  );

  const relationOptions = outwardListRelations.map((rel) => {
    const target = params.schema.find((t) => t.name === rel.targetTable.name);
    assert(target, `Target table ${rel.targetTable.name} not found`);
    return {
      name: rel.name,
      targetColumns: target.columns.map((c) => c.name),
    } as CountRelationsValidatorContext["relationOptions"][number];
  });

  const applyCountRelationsOperationParametersTool = tool(
    async (input: {
      candidateOperationParameters: Record<string, unknown>;
    }) => {
      const result = validateCountRelationsCandidate(
        input.candidateOperationParameters,
        { baseColumns, relationOptions }
      );
      return [result, result];
    },
    {
      name: "applyCountRelationsOperationParametersTool",
      description:
        "Validate and apply the countRelations operation parameters (single final call).",
      schema: z.object({
        candidateOperationParameters: buildCountRelationsZodSchema({
          baseColumns,
          relationOptions,
        }),
      }),
      responseFormat: "content_and_artifact",
    }
  );

  // PROMPT
  const agentPrompt = await getPrompt("extend_query:f100254a");
  const relatedTargetTableNames = relationOptions.map((r) => {
    const outward = params.tableSchema.outwardRelations.find(
      (o) => o.name === r.name
    );
    return outward?.targetTable.name;
  });
  const tablesForSchema = [
    params.tableName,
    ...relatedTargetTableNames.filter(Boolean),
  ];
  const tableSchemasString = tablesForSchema
    .map((tName) => params.schema.find((t) => t.name === tName))
    .filter((tSchema): tSchema is NonNullable<typeof tSchema> =>
      Boolean(tSchema)
    )
    .map((tSchema) => buildTableSchemaStringFromTableSchema(tSchema))
    .join("\n---\n");

  const agentPromptFormatted = (await agentPrompt.invoke({
    operation: params.operation,
    table: params.tableName,
    operationDocs: JSON.stringify(operationDocs[params.operation], null, 2),
    queryCurrentState: "no operation parameters defined",
    tableSchema: tableSchemasString,
    question: params.question,
  })) as Record<"messages", BaseMessage[]>;

  const modelWithTools = model.bindTools([
    applyCountRelationsOperationParametersTool,
  ]);
  type MsgState = typeof MessagesAnnotation.State;

  const callModel = async (state: MsgState) => {
    const response = await modelWithTools.invoke(state.messages);
    return { messages: [response] };
  };

  const jsonDetectedFeedback = async (_state: MsgState) => {
    return {
      messages: new HumanMessage(
        "You produced JSON-like content but did not call applyCountRelationsOperationParametersTool. You MUST now call it exactly once with { candidateOperationParameters: <object> }."
      ),
    };
  };

  const aiMessageContainsJsonLikeText = (msg?: AIMessage) => {
    if (!msg) return false;
    const text = typeof msg.content === "string" ? msg.content : JSON.stringify(msg.content);
    return /\{[\s\S]*\}/.test(text) && !msg.tool_calls?.length;
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
    const last = state.messages.at(-1) as ToolMessageWithArtifact;
    if (
      last &&
      last.name === "applyCountRelationsOperationParametersTool" &&
      last.artifact &&
      last.artifact.status === "valid"
    ) {
      return END;
    }
    return "message_operation_params_agent";
  };

  const workflow = new StateGraph(MessagesAnnotation)
    .addNode("message_operation_params_agent", callModel)
    .addNode("json_detected_feedback", jsonDetectedFeedback)
    .addNode(
      "message_operation_params_agent_tools",
      new ToolNode([applyCountRelationsOperationParametersTool])
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
      }
    );

  const app = workflow.compile();
  const promptMessages = agentPromptFormatted.messages;
  const graphResult = await app.invoke({ messages: promptMessages });

  const validToolMessage = graphResult.messages.toReversed().find((m) => {
    if (!isToolMessage(m)) return false;
    if (m.name !== "applyCountRelationsOperationParametersTool") return false;
    const toolMsg = m as ToolMessageWithArtifact;
    return toolMsg.artifact && toolMsg.artifact.status === "valid" && toolMsg.artifact.result !== undefined;
  }) as ToolMessageWithArtifact | undefined;

  let artifact: unknown = null;
  if (validToolMessage?.artifact && validToolMessage.artifact.status === "valid") {
    artifact = validToolMessage.artifact.result;
  }

  return artifact as CountRelationsQuery["operationParameters"] | undefined;
}
