import assert from "assert";
import { z } from "zod";
import { tool } from "@langchain/core/tools";
import { generateJoinGraph } from "../../utils/tableRelations";
import type { Schema } from "~/server/db/schema";
import type { GroupByQuery } from "~/server/userDatabaseConnector/types";
import {
  validateGroupByCandidate,
  type GroupByValidationResult,
} from "./groupByValidator";
import {
  joinDescriptorSchema,
  joinPathHopSchema,
  joinTypeSchema,
} from "~/server/userDatabaseConnector/types";
import { stringArrayToZodEnum } from "~/server/agents/utils/zodHelpers";
import {
  buildOperationParametersPromptMessages,
  buildOperationParametersTableSchemasString,
  createOperationParametersAgent,
} from "../utils/operationParametersAgent";

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
  const baseTable = params.schema.find(
    (t: Schema[number]) => t.name === params.tableName
  );
  assert(baseTable, "Base table not found");

  const { joinOptions, uniqueTableNames } = generateJoinGraph(
    params.schema,
    params.tableName,
    2
  );

  const tableSchemasString = buildOperationParametersTableSchemasString(
    params.schema,
    uniqueTableNames
  );

  type ColumnMetadata = {
    isTemporal: boolean;
    isNumeric: boolean;
  };

  const columnCatalog = new Map<string, ColumnMetadata>();
  const temporalTypes = new Set(["DateTime", "Date"]);
  const numericTypes = new Set(["number"]);

  uniqueTableNames.forEach((tableName) => {
    const tableSchema = params.schema.find(
      (table: Schema[number]) => table.name === tableName
    );
    if (!tableSchema) return;
    tableSchema.columns.forEach((column: Schema[number]["columns"][number]) => {
      const key = `${tableName}.${column.name}`;
      columnCatalog.set(key, {
        isTemporal: temporalTypes.has(column.type),
        isNumeric: numericTypes.has(column.type),
      });
    });
    tableSchema.computedColumns.forEach(
      (column: NonNullable<Schema[number]["computedColumns"]>[number]) => {
        const key = `${tableName}.${column.name}`;
        columnCatalog.set(key, {
          isTemporal: false,
          isNumeric: true,
        });
      }
    );
  });

  const allColumns = Array.from(columnCatalog.keys());
  const intervalColumns = allColumns.filter(
    (key) => columnCatalog.get(key)?.isTemporal
  );
  const numericalColumns = allColumns.filter(
    (key) => columnCatalog.get(key)?.isNumeric
  );
  const groupableColumns = allColumns.filter(
    (key) => !columnCatalog.get(key)?.isTemporal
  );

  assert(allColumns.length > 0, "No columns available to group by");

  const joinSchema = joinDescriptorSchema
    .extend({
      path: z.array(joinPathHopSchema).min(1),
      joinType: joinTypeSchema.optional(),
    })
    .strip();

  const joinOptionDescriptions = joinOptions
    .map((option) => {
      const pathDescription = option.path
        .map(
          (hop) => `[${hop.source.join(", ")}] -> [${hop.target.join(", ")}]`
        )
        .join(" | ");
      return `${option.name} (${option.table}) path=${pathDescription}`;
    })
    .join("\n");

  const applyGroupByOperationParametersTool = tool(
    async (input: {
      candidateOperationParameters: Record<string, unknown>;
    }) => {
      const result = validateGroupByCandidate(
        input.candidateOperationParameters,
        {
          baseTableName: params.tableName,
          joinOptions,
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
            joins: z.array(joinSchema).nullable().optional(),
            groupBy: z
              .array(
                z.union([
                  z.object({
                    type: z.literal("column"),
                    column: stringArrayToZodEnum(groupableColumns),
                    alias: z.string().min(1).optional(),
                  }),
                  z.object({
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
                  }),
                  z.object({
                    type: z.literal("dateComponent"),
                    column: stringArrayToZodEnum(intervalColumns),
                    component: z.enum([
                      "dayOfWeek",
                      "monthOfYear",
                      "quarterOfYear",
                    ]),
                    alias: z.string().min(1).optional(),
                  }),
                ])
              )
              .min(1),
            count: z.array(stringArrayToZodEnum(allColumns)).nullable(),
            countDistinct: z.array(stringArrayToZodEnum(allColumns)).nullable(),
            sum: z.array(stringArrayToZodEnum(numericalColumns)).nullable(),
            min: z.array(stringArrayToZodEnum(numericalColumns)).nullable(),
            max: z.array(stringArrayToZodEnum(numericalColumns)).nullable(),
            avg: z.array(stringArrayToZodEnum(numericalColumns)).nullable(),
            orderBy: z.union([
              z.object({
                type: z.literal("groupKey"),
                key: z.string(),
                direction: z.enum(["asc", "desc"]),
              }),
              z.object({
                type: z.literal("aggregate"),
                function: z.enum(["count", "countDistinct", "sum", "min", "max", "avg"]),
                column: stringArrayToZodEnum(allColumns),
                direction: z.enum(["asc", "desc"]),
              }),
            ]),
            limit: z.number().int().positive().max(1000),
          })
          .describe(
            joinOptionDescriptions
              ? `Candidate groupBy params object. Available joins:\n${joinOptionDescriptions}`
              : "Candidate groupBy params object; if valid it will be added to the query"
          ),
      }),
      responseFormat: "content_and_artifact",
      verboseParsingErrors: true,
    }
  );

  const messages = await buildOperationParametersPromptMessages(
    params,
    tableSchemasString
  );

  const jsonDetectedMessage =
    "JSON-like content was detected in your last response, but you did not call applyGroupByOperationParametersTool. \n" +
    "You MUST now call applyGroupByOperationParametersTool exactly once with a candidate operationParameters object. \n" +
    "Call the tool with { candidateOperationParameters: <object-or-null> }.";

  const agent = createOperationParametersAgent<
    GroupByQuery["operationParameters"] | null,
    GroupByValidationResult
  >({
    tool: applyGroupByOperationParametersTool,
    toolName: "applyGroupByOperationParametersTool",
    jsonDetectedMessage,
    onComplete: (artifact) => {
      if (artifact?.status !== "valid" || artifact.result === undefined) {
        return null;
      }
      return artifact.result satisfies GroupByQuery["operationParameters"];
    },
  });

  return agent.invoke({ messages });
}
