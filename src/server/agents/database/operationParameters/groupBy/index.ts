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
import { generateJoinedTables } from "../../utils/tableRelations";
import { buildTableSchemaStringFromTableSchema } from "../../utils/schemaFormatters";
import { operationDocs } from "../../utils/operationDocs";
import type { Schema } from "~/server/db/schema";
import type { GroupByQuery } from "~/server/userDatabaseConnector/types";
import {
  validateGroupByCandidate,
  type GroupByValidationResult,
} from "./groupByValidator";
import { stringArrayToZodEnum } from "~/server/agents/utils/zodHelpers";

// Extended ToolMessage type with artifact
interface ToolMessageWithArtifact extends ToolMessage {
  artifact?: GroupByValidationResult;
}

export interface DefineGroupByOperationParametersParams {
  schema: Schema;
  tableName: string;
  question: string;
  tableSchema: Schema[number];
  operation: "groupBy";
}

export async function defineGroupByOperationParameters(
  params: DefineGroupByOperationParametersParams
) {
  const model = getAIModel("azure:gpt-5");

  const tableCandidates = generateJoinedTables(
    params.schema,
    params.tableName,
    1
  );
  assert(tableCandidates, "Failed to generate join candidates");

  const baseTable = params.schema.find((t) => t.name === params.tableName);
  assert(baseTable, "Base table not found");

  const joinOptions = Object.entries(tableCandidates.iqlPaths)
    .filter(([, tName]) => tName !== params.tableName)
    .map(([joinPath, tName]) => ({ joinPath, table: tName }));

  const tablesPotential = new Set<string>([
    params.tableName,
    ...joinOptions.map((o) => o.table),
  ]);

  type ColumnMetadata = {
    isTemporal: boolean;
    isNumeric: boolean;
  };

  const columnCatalog = new Map<string, ColumnMetadata>();

  const upsertColumn = (columnKey: string, metadata: ColumnMetadata) => {
    const existing = columnCatalog.get(columnKey);
    if (!existing) {
      columnCatalog.set(columnKey, metadata);
      return;
    }
    columnCatalog.set(columnKey, {
      isTemporal: existing.isTemporal || metadata.isTemporal,
      isNumeric: existing.isNumeric || metadata.isNumeric,
    });
  };

  const temporalTypes = new Set(["DateTime", "Date"]);
  const numericTypes = new Set(["number"]);

  tablesPotential.forEach((tName) => {
    const tSchema = params.schema.find((t) => t.name === tName);
    if (!tSchema) return;
    tSchema.columns.forEach((c) => {
      const fq = `${tName}.${c.name}`;
      upsertColumn(fq, {
        isTemporal: temporalTypes.has(c.type),
        isNumeric: numericTypes.has(c.type),
      });
    });
    tSchema.computedColumns.forEach((c) => {
      const fq = `${tName}.${c.name}`;
      upsertColumn(fq, {
        isTemporal: false,
        isNumeric: true, // computed columns are assumed numeric for aggregation eligibility
      });
    });
  });

  const allColumns = Array.from(columnCatalog.keys());
  const intervalColumns = allColumns.filter(
    (col) => columnCatalog.get(col)?.isTemporal === true
  );
  const numericalColumns = allColumns.filter(
    (col) => columnCatalog.get(col)?.isNumeric === true
  );
  const groupableColumns = allColumns.filter(
    (col) => columnCatalog.get(col)?.isTemporal !== true
  );

  assert(
    allColumns.length > 0,
    "No columns available to group by in selected tables"
  );

  // TOOL: validate + apply groupBy operation parameters
  const applyGroupByOperationParametersTool = tool(
    async (input: {
      candidateOperationParameters: Record<string, unknown>;
    }) => {
      const result = validateGroupByCandidate(
        input.candidateOperationParameters,
        {
          baseTableName: params.tableName,
          joinOptions: joinOptions.map((o) => ({
            table: o.table,
            joinPath: o.joinPath,
          })),
          allColumns,
          groupableColumns,
          intervalColumns,
          numericalColumns,
        }
      );
      return [result, result];
    },
    {
      name: "applyGroupByOperationParametersTool",
      description:
        "Validate and apply the groupBy operation parameters (single final call).",
      schema: z.object({
        candidateOperationParameters: z
          .object({
            joins: z
              .array(
                z
                  .object({
                    table: z.string(),
                    joinPath: z.string(),
                    joinType: z.enum(["inner", "left", "right", "full"]),
                  })
                  .describe(
                    `The possible join paths are: \n${Object.keys(
                      tableCandidates.iqlPaths
                    ).join(", ")}.`
                  )
              )
              .nullable()
              .describe(
                `The tables to join to the ${params.tableName} table, if any.`
              ),
            groupBy: (() => {
              const keySchemas: z.ZodTypeAny[] = [];
              if (groupableColumns.length > 0) {
                keySchemas.push(
                  z
                    .object({
                      type: z.literal("column"),
                      column: stringArrayToZodEnum(groupableColumns),
                      alias: z.string().min(1).optional(),
                    })
                    .strict()
                );
              }
              if (intervalColumns.length > 0) {
                keySchemas.push(
                  z
                    .object({
                      type: z.literal("dateInterval"),
                      column: stringArrayToZodEnum(intervalColumns),
                      interval: z.enum([
                        "day",
                        "week",
                        "month",
                        "quarter",
                        "year",
                        "hour",
                      ]),
                      alias: z.string().min(1).optional(),
                    })
                    .strict()
                );
              }
              const unionSchema = keySchemas[0]
                ? keySchemas.length === 1
                  ? keySchemas[0]
                  : z.union(
                      keySchemas as [
                        z.ZodTypeAny,
                        z.ZodTypeAny,
                        ...z.ZodTypeAny[]
                      ]
                    )
                : z.never();
              return z.array(unionSchema).min(1);
            })(),
            count: (() => {
              if (!allColumns.length) return z.null();
              return z
                .array(stringArrayToZodEnum(allColumns))
                .min(1)
                .describe("Columns to count (fully qualified)")
                .nullable();
            })(),
            sum: (() => {
              if (!numericalColumns.length) return z.null();
              return z
                .array(stringArrayToZodEnum(numericalColumns))
                .min(1)
                .describe("Columns to sum (fully qualified)")
                .nullable();
            })(),
            min: (() => {
              if (!numericalColumns.length) return z.null();
              return z
                .array(stringArrayToZodEnum(numericalColumns))
                .min(1)
                .describe("Columns to take minimum over (fully qualified)")
                .nullable();
            })(),
            max: (() => {
              if (!numericalColumns.length) return z.null();
              return z
                .array(stringArrayToZodEnum(numericalColumns))
                .min(1)
                .describe("Columns to take maximum over (fully qualified)")
                .nullable();
            })(),
            avg: (() => {
              if (!numericalColumns.length) return z.null();
              return z
                .array(stringArrayToZodEnum(numericalColumns))
                .min(1)
                .describe("Columns to average (fully qualified)")
                .nullable();
            })(),
            orderBy: z.union([
              z
                .object({
                  type: z.literal("aggregate"),
                  function: z.enum(
                    numericalColumns.length
                      ? (["count", "sum", "min", "max", "avg"] as const)
                      : (["count"] as const)
                  ),
                  column: stringArrayToZodEnum(allColumns),
                  direction: z.enum(["asc", "desc"]),
                })
                .strict(),
              z
                .object({
                  type: z.literal("groupKey"),
                  key: z.string(),
                  direction: z.enum(["asc", "desc"]),
                })
                .strict(),
            ]),
            limit: z.number().min(1).max(1000),
          })
          .describe(
            "Candidate groupBy params object; must include groupBy, orderBy, limit, and optional aggregates"
          ),
      }),
      responseFormat: "content_and_artifact",
    }
  );

  // PROMPT
  const agentPrompt = await getPrompt("extend_query:f100254a");
  const tableSchemasString = Array.from(tablesPotential)
    .map((tName) => {
      const ts = params.schema.find((t) => t.name === tName);
      return ts
        ? buildTableSchemaStringFromTableSchema(ts)
        : `# Missing schema for ${tName}`;
    })
    .join("\n---\n");

  const agentPromptFormatted = (await agentPrompt.invoke({
    operation: params.operation,
    table: params.tableName,
    operationDocs: JSON.stringify(operationDocs[params.operation], null, 2),
    queryCurrentState: "no operation parameters defined",
    tableSchema: tableSchemasString,
    question: params.question,
  })) as Record<"messages", BaseMessage[]>;

  const modelWithTools = model.bindTools([applyGroupByOperationParametersTool]);
  type MsgState = typeof MessagesAnnotation.State;

  const callModel = async (state: MsgState) => {
    const response = await modelWithTools.invoke(state.messages);
    return { messages: [response] };
  };

  const jsonDetectedFeedback = async (_state: MsgState) => {
    return {
      messages: new HumanMessage(
        "You produced JSON-like content but did not call applyGroupByOperationParametersTool. You MUST now call it exactly once with { candidateOperationParameters: <object> }."
      ),
    };
  };

  // Naive JSON detection similar to findMany util (inline simple check)
  const aiMessageContainsJsonLikeText = (msg?: AIMessage) => {
    if (!msg) return false;
    const text =
      typeof msg.content === "string"
        ? msg.content
        : JSON.stringify(msg.content);
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
      last.name === "applyGroupByOperationParametersTool" &&
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
      new ToolNode([applyGroupByOperationParametersTool])
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
    if (m.name !== "applyGroupByOperationParametersTool") return false;
    const toolMsg = m as ToolMessageWithArtifact;
    return (
      toolMsg?.artifact?.status === "valid" &&
      toolMsg.artifact.result !== undefined
    );
  }) as ToolMessageWithArtifact | undefined;

  let artifact: unknown = null;
  if (validToolMessage?.artifact?.status === "valid") {
    artifact = validToolMessage.artifact.result;
  }

  return artifact as GroupByQuery["operationParameters"] | undefined;
}
