import assert from "assert";
import { z } from "zod";
import { tool } from "@langchain/core/tools";
import type { Schema } from "~/server/db/schema";
import type { CountQuery } from "~/server/userDatabaseConnector/types";
import { generateJoinGraph } from "../../utils/tableRelations";
import {
  buildCountToolZodSchema,
  validateCountCandidate,
  type CountValidatorContext,
  type CountValidationResult,
} from "./countValidator";
import {
  buildOperationParametersPromptMessages,
  buildOperationParametersTableSchemasString,
  createOperationParametersAgent,
} from "../utils/operationParametersAgent";

export interface DefineCountOperationParametersParams {
  schema: Schema;
  tableName: string;
  question: string;
  tableSchema: Schema[number];
  operation: "count";
}

export async function defineCountOperationParameters(
  params: DefineCountOperationParametersParams
) {
  const baseTableSchema = params.tableSchema;
  const baseColumns = baseTableSchema.columns.map(
    (column: Schema[number]["columns"][number]) => column.name
  );
  const computedColumns = baseTableSchema.computedColumns ?? [];

  const joinGraph = generateJoinGraph(params.schema, params.tableName, 2);
  const joinOptions = joinGraph.joinOptions;

  const validatorContext: CountValidatorContext = {
    baseTable: params.tableName,
    baseColumns,
    computedColumns: computedColumns.map((column) => column.name),
    joinOptions,
  };

  const applyCountOperationParametersTool = tool(
    async (input: {
      candidateOperationParameters: Record<string, unknown>;
    }) => {
      const result = validateCountCandidate(
        input.candidateOperationParameters,
        validatorContext
      );
      return [result, result];
    },
    {
      name: "applyCountOperationParametersTool",
      description:
        "Validate and apply the count operation parameters (single final call).",
      schema: z.object({
        candidateOperationParameters: buildCountToolZodSchema(validatorContext),
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
    CountQuery["operationParameters"],
    CountValidationResult
  >({
    tool: applyCountOperationParametersTool,
    toolName: "applyCountOperationParametersTool",
    onComplete: (artifact) => {
      assert(
        artifact?.status === "valid" && artifact.result,
        "Failed to produce valid count operation parameters"
      );
      return artifact.result;
    },
  });

  const result = await agent.invoke({ messages });
  return result satisfies CountQuery["operationParameters"];
}
