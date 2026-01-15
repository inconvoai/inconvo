import { z } from "zod";
import { tool } from "@langchain/core/tools";
import assert from "assert";
import { generateJoinGraph } from "../../utils/tableRelations";
import {
  buildOperationParametersPromptMessages,
  buildOperationParametersTableSchemasString,
  createOperationParametersAgent,
} from "../utils/operationParametersAgent";
import { stringArrayToZodEnum } from "../../../utils/zodHelpers";
import { buildPromptCacheKey } from "../../../utils/promptCacheKey";
import {
  validateFindManyCandidate,
  type FindManyValidationResult,
} from "./findManyValidator";
import type { Schema } from "@repo/types";
import {
  findManySchema,
  joinDescriptorSchema,
  joinPathHopSchema,
  type FindManyQuery,
} from "@repo/types";

export interface DefineFindManyOperationParametersParams {
  schema: Schema;
  tableName: string;
  question: string;
  tableSchema: Schema[number];
  operation: "findMany";
  userContext: Record<string, string | number>;
  agentId: string | number;
}

export async function defineFindManyOperationParameters(
  params: DefineFindManyOperationParametersParams,
) {
  const baseTableSchema = params.schema.find(
    (t: Schema[number]) => t.name === params.tableName,
  );
  assert(baseTableSchema, `Base table ${params.tableName} not found`);
  const baseColumns = baseTableSchema.columns.map(
    (c: Schema[number]["columns"][number]) => c.name,
  );
  const baseComputedColumns = baseTableSchema.computedColumns.map(
    (c: NonNullable<Schema[number]["computedColumns"]>[number]) => c.name,
  );

  const { aliasToTable, joinOptions, uniqueTableNames } = generateJoinGraph(
    params.schema,
    params.tableName,
    2,
  );
  assert(uniqueTableNames, "Failed to generate unique table names");

  const tableSchemasString = buildOperationParametersTableSchemasString(
    params.schema,
    uniqueTableNames,
  );

  const selectableTableColumns: Record<string, string[]> = Object.entries(
    aliasToTable,
  ).reduce<Record<string, string[]>>((acc, [alias, tableName]) => {
    const tableSchema = params.schema.find(
      (table: Schema[number]) => table.name === tableName,
    );
    if (!tableSchema) return acc;
    acc[alias] = [
      ...tableSchema.columns.map(
        (column: Schema[number]["columns"][number]) => column.name,
      ),
      ...tableSchema.computedColumns.map(
        (column: NonNullable<Schema[number]["computedColumns"]>[number]) =>
          column.name,
      ),
    ];
    return acc;
  }, {});

  const joinSchema = joinDescriptorSchema
    .omit({ joinType: true })
    .extend({
      path: z.array(joinPathHopSchema).min(1),
    })
    .strict();

  const joinOptionDescriptions = joinOptions
    .map((option) => {
      const pathDescription = option.path
        .map(
          (hop) => `[${hop.source.join(", ")}] -> [${hop.target.join(", ")}]`,
        )
        .join(" | ");
      return `${option.name} (${option.table}) path=${pathDescription}`;
    })
    .join("\n");

  const applyFindManyOperationParametersTool = tool(
    async (input: {
      candidateOperationParameters: Record<string, unknown>;
    }) => {
      const result = validateFindManyCandidate(
        input.candidateOperationParameters,
        {
          baseTable: params.tableName,
          selectableTableColumns,
          baseColumns,
          baseComputedColumns,
          joinOptions,
        },
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
            select: z.object(
              Object.keys(selectableTableColumns).reduce(
                (
                  acc: Record<string, z.ZodOptional<z.ZodArray<z.ZodString>>>,
                  key,
                ) => {
                  acc[key] = z.array(z.string()).optional();
                  return acc;
                },
                {} as Record<string, z.ZodOptional<z.ZodArray<z.ZodString>>>,
              ),
            ),
            joins: z.array(joinSchema).nullable().optional(),
            orderBy: z
              .object({
                direction: z.enum(["asc", "desc"]),
                column: stringArrayToZodEnum([
                  ...baseColumns,
                  ...baseComputedColumns,
                ]).describe(
                  "The column to order by [Only columns from base table]",
                ),
              })
              .nullable(),
            limit: z.number().int().positive().max(1000),
          })
          .describe(
            joinOptionDescriptions
              ? `Candidate findMany params object. Available joins:\n${joinOptionDescriptions}`
              : "Candidate findMany params object; if valid it will be added to the query",
          ),
      }),
      responseFormat: "content_and_artifact",
      verboseParsingErrors: true,
    },
  );

  const messages = await buildOperationParametersPromptMessages(
    params,
    tableSchemasString,
  );

  const jsonDetectedMessage =
    "JSON-like content was detected in your last response, but you did not call applyFindManyOperationParametersTool. \n" +
    "You MUST now call applyFindManyOperationParametersTool exactly once with a candidate operationParameters object. \n" +
    "Call the tool with { candidateOperationParameters: <object-or-null> }.";

  const agent = createOperationParametersAgent<
    FindManyQuery["operationParameters"] | null,
    FindManyValidationResult
  >({
    promptCacheKey: buildPromptCacheKey({
      agentId: params.agentId,
      userContext: params.userContext,
    }),
    tool: applyFindManyOperationParametersTool,
    toolName: "applyFindManyOperationParametersTool",
    jsonDetectedMessage,
    onComplete: (artifact) => {
      if (artifact?.status !== "valid" || !artifact.result) {
        return null;
      }
      const parsed = findManySchema.shape.operationParameters.parse({
        select: artifact.result.select,
        joins: artifact.result.joins ?? null,
        orderBy: artifact.result.orderBy,
        limit: artifact.result.limit,
      });
      return parsed satisfies FindManyQuery["operationParameters"];
    },
  });

  return agent.invoke({ messages });
}
