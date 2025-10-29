import { z } from "zod";
import { tool } from "@langchain/core/tools";
import {
  type AIMessage,
  HumanMessage,
  isToolMessage,
  type BaseMessage,
  type ToolMessage,
} from "@langchain/core/messages";
import assert from "assert";
import { getAIModel } from "~/server/agents/utils/getAIModel";
import { generateJoinedTables } from "../../utils/tableRelations";
import { buildTableSchemaStringFromTableSchema } from "../../utils/schemaFormatters";
import { stringArrayToZodEnum } from "../../../utils/zodHelpers";
import {
  validateFindManyCandidate,
  type FindManyValidationResult,
} from "./findManyValidator";
import { operationDocs } from "../../utils/operationDocs";
import type { Schema } from "~/server/db/schema";
import { findManySchema } from "~/server/userDatabaseConnector/types";
import {
  END,
  MessagesAnnotation,
  START,
  StateGraph,
} from "@langchain/langgraph";
import { getPrompt } from "~/server/agents/utils/getPrompt";
import { aiMessageContainsJsonLikeText } from "~/server/agents/utils/langchainMessageUtils";
import { ToolNode } from "@langchain/langgraph/prebuilt";

// Extended ToolMessage type with artifact
interface ToolMessageWithArtifact extends ToolMessage {
  artifact?: FindManyValidationResult;
}

export interface DefineFindManyOperationParametersParams {
  schema: Schema;
  tableName: string;
  question: string;
  tableSchema: Schema[number];
  operation: "findMany";
}

export async function defineFindManyOperationParameters(
  params: DefineFindManyOperationParametersParams
) {
  const model = getAIModel("azure:gpt-5");
  const baseTableSchema = params.schema.find(
    (t) => t.name === params.tableName
  );
  assert(baseTableSchema, `Base table ${params.tableName} not found`);
  const baseColumns = baseTableSchema.columns.map((c) => c.name);
  const baseComputedColumns = baseTableSchema.computedColumns.map(
    (c) => c.name
  );

  const { iqlPaths, uniqueTableNames } = generateJoinedTables(
    params.schema,
    params.tableName,
    2
  );
  assert(iqlPaths, "Failed to generate joined tables");
  assert(uniqueTableNames, "Failed to generate unique table names");

  const tableSchemasString = uniqueTableNames
    .map((tName) => {
      const ts = params.schema.find((t) => t.name === tName);
      return ts
        ? buildTableSchemaStringFromTableSchema(ts)
        : `# Missing schema for ${tName}`;
    })
    .join("\n---\n");

  const selectableTableColumns: Record<string, string[]> = {};
  Object.entries(iqlPaths).forEach(([path, tName]) => {
    const tSchema = params.schema.find((t) => t.name === tName);
    if (!tSchema) return;
    selectableTableColumns[path] = [
      ...tSchema.columns.map((c) => c.name),
      ...tSchema.computedColumns.map((c) => c.name),
    ];
  });

  const applyFindManyOperationParametersTool = tool(
    async (input: {
      candidateOperationParameters: Record<string, unknown>;
    }) => {
      const result = validateFindManyCandidate(
        input.candidateOperationParameters,
        {
          selectableTableColumns,
          baseColumns,
          baseComputedColumns,
        }
      );
      return [result, result];
    },
    {
      name: "applyFindManyOperationParametersTool",
      description:
        "Validate and apply the findMany operation parameters (single final call).",
      schema: z.object({
        candidateOperationParameters: z
          .object({
            columns: z.object(
              Object.keys(iqlPaths).reduce(
                (
                  acc: Record<string, z.ZodOptional<z.ZodArray<z.ZodString>>>,
                  key
                ) => {
                  acc[key] = z.array(z.string()).optional();
                  return acc;
                },
                {} as Record<string, z.ZodOptional<z.ZodArray<z.ZodString>>>
              )
            ),
            orderBy: z
              .object({
                direction: z.enum(["asc", "desc"]),
                column: stringArrayToZodEnum([
                  ...baseColumns,
                  ...baseComputedColumns,
                ]).describe(
                  "The column to order by [Only columns from base table]"
                ),
              })
              .nullable(),
            limit: z.number().int().positive().max(1000),
          })
          .describe(
            "Candidate findMany params object; if valid it will be added to the query"
          ),
      }),
      responseFormat: "content_and_artifact",
      verboseParsingErrors: true,
    }
  );

  const agentPrompt = await getPrompt("extend_query:f100254a");
  const agentPromptFormatted = (await agentPrompt.invoke({
    operation: params.operation,
    table: params.tableName,
    operationDocs: JSON.stringify(operationDocs[params.operation], null, 2),
    queryCurrentState: "no operation parameters defined",
    tableSchema: tableSchemasString,
    question: params.question,
  })) as Record<"messages", BaseMessage[]>;

  const modelWithTools = model.bindTools([
    applyFindManyOperationParametersTool,
  ]);
  type MsgState = typeof MessagesAnnotation.State;

  const callModel = async (state: MsgState) => {
    const response = await modelWithTools.invoke(state.messages);
    return { messages: [response] };
  };

  const jsonDetectedFeedback = async (_state: MsgState) => {
    return {
      messages: new HumanMessage(
        "JSON-like content was detected in your last response, but you did not call applyFindManyOperationParametersTool. \n" +
          "You MUST now call applyFindManyOperationParametersTool exactly once with a candidate operationParameters object. \n" +
          "Call the tool with { candidateOperationParameters: <object-or-null> }."
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
    const last = state.messages.at(-1) as ToolMessageWithArtifact;
    if (
      last.name === "applyFindManyOperationParametersTool" &&
      last.artifact?.status === "valid"
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
      new ToolNode([applyFindManyOperationParametersTool])
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
    if (m.name !== "applyFindManyOperationParametersTool") return false;
    const toolMsg = m as ToolMessageWithArtifact;
    return (
      toolMsg.artifact?.status === "valid" &&
      toolMsg.artifact.result !== undefined
    );
  }) as ToolMessageWithArtifact | undefined;

  let artifact: unknown = null;
  if (validToolMessage?.artifact?.status === "valid") {
    artifact = validToolMessage.artifact.result;
  }

  const parsed = findManySchema.shape.operationParameters.safeParse(artifact);
  return parsed.data;
}
