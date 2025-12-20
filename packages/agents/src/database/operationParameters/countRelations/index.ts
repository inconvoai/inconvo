import assert from "assert";
import { z } from "zod";
import { tool } from "@langchain/core/tools";
import { buildTableSchemaStringFromTableSchema } from "../../utils/schemaFormatters";
import type { Schema } from "@repo/types";
import type { CountRelationsQuery } from "@repo/types";
import {
  validateCountRelationsCandidate,
  buildCountRelationsZodSchema,
} from "./countRelationsValidator";
import type {
  CountRelationsValidatorContext,
  CountRelationsValidationResult,
} from "./countRelationsValidator";
import {
  buildOperationParametersPromptMessages,
  createOperationParametersAgent,
} from "../utils/operationParametersAgent";

export interface DefineCountRelationsOperationParametersParams {
  schema: Schema;
  tableName: string;
  question: string;
  tableSchema: Schema[number];
  operation: "countRelations";
}

export async function defineCountRelationsOperationParameters(
  params: DefineCountRelationsOperationParametersParams,
) {
  // Gather base columns
  const baseTable = params.schema.find(
    (t: Schema[number]) => t.name === params.tableName,
  );
  assert(baseTable, "Base table not found");
  const baseColumns = baseTable.columns.map(
    (c: Schema[number]["columns"][number]) => c.name,
  );

  // Outward list relations only
  const outwardListRelations = (
    params.tableSchema.outwardRelations ?? []
  ).filter(
    (relation: Schema[number]["outwardRelations"][number]) => relation.isList,
  );

  const relationOptions: CountRelationsValidatorContext["relationOptions"] = [];

  outwardListRelations.forEach(
    (rel: Schema[number]["outwardRelations"][number]) => {
      const target = params.schema.find(
        (t: Schema[number]) => t.name === rel.targetTable.name,
      );
      if (!target) {
        console.warn(
          `Target table ${rel.targetTable.name} not found for relation ${rel.name}`,
        );
        return;
      }

      const sourceColumns = rel.sourceColumns ?? [];
      const targetColumns = rel.targetColumns ?? [];

      if (sourceColumns.length === 0 || targetColumns.length === 0) {
        console.warn(
          `Relation ${rel.name} is missing column metadata and cannot be used for countRelations joins`,
        );
        return;
      }

      relationOptions.push({
        name: rel.name,
        table: rel.targetTable.name,
        path: [
          {
            source: sourceColumns.map(
              (column) => `${params.tableName}.${column}`,
            ),
            target: targetColumns.map(
              (column) => `${rel.targetTable.name}.${column}`,
            ),
          },
        ],
        targetColumns: target.columns.map(
          (c: Schema[number]["columns"][number]) => c.name,
        ),
      });
    },
  );

  const applyCountRelationsOperationParametersTool = tool(
    async (input: {
      candidateOperationParameters: Record<string, unknown>;
    }) => {
      const result = validateCountRelationsCandidate(
        input.candidateOperationParameters,
        { baseColumns, relationOptions },
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
      verboseParsingErrors: true,
    },
  );

  const relatedTargetTableNames = relationOptions.map((r) => {
    const outward = (params.tableSchema.outwardRelations ?? []).find(
      (o: Schema[number]["outwardRelations"][number]) => o.name === r.name,
    );
    return outward?.targetTable.name;
  });
  const tablesForSchema = [
    params.tableName,
    ...relatedTargetTableNames.filter(Boolean),
  ];
  const tableSchemasString = tablesForSchema
    .map((tName) => params.schema.find((t: Schema[number]) => t.name === tName))
    .filter((tSchema): tSchema is NonNullable<typeof tSchema> =>
      Boolean(tSchema),
    )
    .map((tSchema) => buildTableSchemaStringFromTableSchema(tSchema))
    .join("\n---\n");

  const messages = await buildOperationParametersPromptMessages(
    params,
    tableSchemasString,
  );

  const agent = createOperationParametersAgent<
    CountRelationsQuery["operationParameters"] | undefined,
    CountRelationsValidationResult
  >({
    tool: applyCountRelationsOperationParametersTool,
    toolName: "applyCountRelationsOperationParametersTool",
    jsonDetectedMessage:
      "You produced JSON-like content but did not call applyCountRelationsOperationParametersTool. You MUST now call it exactly once with { candidateOperationParameters: <object> }.",
    onComplete: (artifact) => {
      if (artifact?.status !== "valid" || artifact.result === undefined) {
        return undefined;
      }
      return artifact.result;
    },
  });

  return agent.invoke({ messages });
}
