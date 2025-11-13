import assert from "assert";
import { z } from "zod";
import { tool } from "@langchain/core/tools";
import type { Schema } from "~/server/db/schema";
import { buildTableSchemaStringFromTableSchema } from "../../utils/schemaFormatters";
import type { FindDistinctQuery } from "~/server/userDatabaseConnector/types";
import {
  buildFindDistinctToolZodSchema,
  validateFindDistinctCandidate,
  type FindDistinctValidationResult,
  type FindDistinctValidatorContext,
} from "./findDistinctValidator";
import {
  buildOperationParametersPromptMessages,
  createOperationParametersAgent,
} from "../utils/operationParametersAgent";

export interface DefineFindDistinctOperationParametersParams {
  schema: Schema;
  tableName: string;
  question: string;
  tableSchema: Schema[number];
  operation: "findDistinct";
}

export async function defineFindDistinctOperationParameters(
  params: DefineFindDistinctOperationParametersParams
) {
  const baseTableSchema = params.schema.find(
    (table: Schema[number]) => table.name === params.tableName
  );
  assert(baseTableSchema, "Base table not found");

  const selectableColumns = [
    ...baseTableSchema.columns.map(
      (column: Schema[number]["columns"][number]) =>
        `${params.tableName}.${column.name}`
    ),
    ...(baseTableSchema.computedColumns ?? []).map(
      (column: NonNullable<Schema[number]["computedColumns"]>[number]) =>
        `${params.tableName}.${column.name}`
    ),
  ];
  const validatorContext: FindDistinctValidatorContext = {
    baseTable: params.tableName,
    selectableColumns,
  };

  const applyFindDistinctOperationParametersTool = tool(
    async (input: {
      candidateOperationParameters: Record<string, unknown>;
    }) => {
      const result = validateFindDistinctCandidate(
        input.candidateOperationParameters,
        validatorContext
      );
      return [result, result];
    },
    {
      name: "applyFindDistinctOperationParametersTool",
      description:
        "Validate and apply the findDistinct operation parameters (single final call).",
      schema: z.object({
        candidateOperationParameters: buildFindDistinctToolZodSchema(),
      }),
      responseFormat: "content_and_artifact",
      verboseParsingErrors: true,
    }
  );

  const tableSchemaString = buildTableSchemaStringFromTableSchema(
    params.tableSchema
  );

  const messages = await buildOperationParametersPromptMessages(
    params,
    tableSchemaString
  );

  const agent = createOperationParametersAgent<
    FindDistinctQuery["operationParameters"],
    FindDistinctValidationResult
  >({
    tool: applyFindDistinctOperationParametersTool,
    toolName: "applyFindDistinctOperationParametersTool",
    onComplete: (artifact) => {
      assert(
        artifact?.status === "valid" && artifact.result,
        "Failed to produce valid findDistinct operation parameters"
      );
      return artifact.result;
    },
  });

  const result = await agent.invoke({ messages });
  return result satisfies FindDistinctQuery["operationParameters"];
}
