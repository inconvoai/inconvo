import assert from "assert";
import { z } from "zod";
import { tool } from "@langchain/core/tools";
import type { Schema } from "~/server/db/schema";
import {
  generateJoinGraph,
  type GeneratedJoinOption,
} from "../../utils/tableRelations";
import type { AggregateQuery } from "~/server/userDatabaseConnector/types";
import {
  buildAggregateToolZodSchema,
  validateAggregateCandidate,
  type AggregateColumnCatalog,
  type AggregateValidatorContext,
  type AggregateValidationResult,
  type AggregateColumnMetadata,
} from "./aggregateValidator";
import {
  buildOperationParametersPromptMessages,
  buildOperationParametersTableSchemasString,
  createOperationParametersAgent,
} from "../utils/operationParametersAgent";

export interface DefineAggregateOperationParametersParams {
  schema: Schema;
  tableName: string;
  question: string;
  tableSchema: Schema[number];
  operation: "aggregate";
}

export async function defineAggregateOperationParameters(
  params: DefineAggregateOperationParametersParams
) {
  const joinGraph = generateJoinGraph(params.schema, params.tableName, 2);
  const columnCatalog = buildColumnCatalog(
    params.schema,
    params.tableName,
    params.tableSchema,
    joinGraph.joinOptions
  );

  const validatorContext: AggregateValidatorContext = {
    baseTable: params.tableName,
    joinOptions: joinGraph.joinOptions,
    columnCatalog,
  };

  const applyAggregateOperationParametersTool = tool(
    async (input: {
      candidateOperationParameters: Record<string, unknown>;
    }) => {
      const result = validateAggregateCandidate(
        input.candidateOperationParameters,
        validatorContext
      );
      return [result, result];
    },
    {
      name: "applyAggregateOperationParametersTool",
      description:
        "Validate and apply the aggregate operation parameters (single final call).",
      schema: z.object({
        candidateOperationParameters:
          buildAggregateToolZodSchema(validatorContext),
      }),
      responseFormat: "content_and_artifact",
      verboseParsingErrors: true,
    }
  );

  const tableSchemasString = buildOperationParametersTableSchemasString(
    params.schema,
    joinGraph.uniqueTableNames
  );
  const messages = await buildOperationParametersPromptMessages(
    params,
    tableSchemasString
  );

  const agent = createOperationParametersAgent<
    AggregateQuery["operationParameters"],
    AggregateValidationResult
  >({
    tool: applyAggregateOperationParametersTool,
    toolName: "applyAggregateOperationParametersTool",
    onComplete: (artifact) => {
      assert(
        artifact?.status === "valid" && artifact.result,
        "Failed to produce valid aggregate operation parameters"
      );
      return artifact.result;
    },
  });

  const result = await agent.invoke({ messages });
  return result satisfies AggregateQuery["operationParameters"];
}

function buildColumnCatalog(
  schema: Schema,
  baseTableName: string,
  baseTableSchema: Schema[number],
  joinOptions: GeneratedJoinOption[]
): AggregateColumnCatalog {
  const catalog: AggregateColumnCatalog = {};

  const register = (
    alias: string,
    columnName: string,
    metadata: AggregateColumnMetadata
  ) => {
    catalog[alias] ??= {};
    const existing = catalog[alias][columnName];
    if (existing) {
      catalog[alias][columnName] = {
        isNumeric: existing.isNumeric || metadata.isNumeric,
        isTemporal: existing.isTemporal || metadata.isTemporal,
        isCountable: existing.isCountable || metadata.isCountable,
      };
      return;
    }
    catalog[alias][columnName] = metadata;
  };

  const numericTypes = new Set(["number"]);
  const temporalTypes = new Set(["DateTime", "Date"]);

  baseTableSchema.columns.forEach(
    (column: Schema[number]["columns"][number]) => {
      register(baseTableName, column.name, {
        isNumeric: numericTypes.has(column.type),
        isTemporal: temporalTypes.has(column.type),
        isCountable: true,
      });
    }
  );

  (baseTableSchema.computedColumns ?? []).forEach(
    (column: NonNullable<Schema[number]["computedColumns"]>[number]) => {
      register(baseTableName, column.name, {
        isNumeric: column.type === "number",
        isTemporal: false,
        isCountable: true,
      });
    }
  );

  const schemaByName = new Map(
    schema.map((table: Schema[number]) => [table.name, table])
  );

  joinOptions.forEach((option) => {
    const tableSchema = schemaByName.get(option.table);
    if (!tableSchema) return;

    tableSchema.columns.forEach((column: Schema[number]["columns"][number]) => {
      register(option.name, column.name, {
        isNumeric: numericTypes.has(column.type),
        isTemporal: temporalTypes.has(column.type),
        isCountable: true,
      });
    });

    (tableSchema.computedColumns ?? []).forEach(
      (column: NonNullable<Schema[number]["computedColumns"]>[number]) => {
        register(option.name, column.name, {
          isNumeric: column.type === "number",
          isTemporal: false,
          isCountable: true,
        });
      }
    );
  });

  return catalog;
}
