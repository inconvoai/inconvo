import { Kysely } from "kysely";
import type { Query } from "../../types/querySchema";
import { buildWhereConditions } from "../utils/whereConditionBuilder";
import { getColumnFromTable } from "../utils/computedColumns";
import { applyLimit } from "../utils/queryHelpers";
import { getSchemaBoundDb } from "../utils/schemaHelpers";
import { resolveBaseSource } from "../utils/logicalTableSource";
import { executeWithLogging } from "../utils/executeWithLogging";
import type { OperationContext } from "../types";
import levenshtein from "fast-levenshtein";
import assert from "assert";

function filterStringByEditDistance(
  strings: string[],
  targetString: string,
  maxResults: number,
) {
  const stringWithEditDistanceToTarget = strings.map((str) => ({
    str,
    distance: levenshtein.get(str, targetString),
  }));
  stringWithEditDistanceToTarget.sort((a, b) => a.distance - b.distance);
  return stringWithEditDistanceToTarget
    .slice(0, maxResults)
    .map((item) => item.str);
}

function getDeclaredColumnTypeForEditDistance(
  ctx: OperationContext,
  tableName: string,
  columnName: string,
) {
  const table = ctx.schema.tables.find(
    (candidate) => candidate.name === tableName,
  );
  if (!table) {
    return undefined;
  }

  const computedColumn = table.computedColumns?.find(
    (candidate) => candidate.name === columnName,
  );
  if (computedColumn) {
    return computedColumn.type ?? undefined;
  }

  const columnConversion = table.columnConversions?.find(
    (candidate) => candidate.column === columnName,
  );
  if (columnConversion) {
    return columnConversion.type ?? undefined;
  }

  return table.columns.find((candidate) => candidate.name === columnName)?.type;
}

function assertColumnSupportsEditDistance(
  ctx: OperationContext,
  tableName: string,
  columnName: string,
) {
  const declaredType = getDeclaredColumnTypeForEditDistance(
    ctx,
    tableName,
    columnName,
  );
  if (!declaredType) {
    return;
  }

  if (declaredType.toLowerCase() !== "string") {
    throw new Error(
      `findDistinctByEditDistance requires a string column, but '${tableName}.${columnName}' is type '${declaredType}'.`,
    );
  }
}

function extractStringValuesForEditDistance(
  values: Array<Record<string, unknown>>,
  responseColumn: string,
) {
  const nonNullValues = values
    .map((value) => value[responseColumn])
    .filter(
      (value): value is NonNullable<unknown> =>
        value !== null && value !== undefined,
    );

  const stringValues = nonNullValues.filter(
    (value): value is string => typeof value === "string",
  );
  if (stringValues.length !== nonNullValues.length) {
    const firstNonString = nonNullValues.find(
      (value) => typeof value !== "string",
    );
    const nonStringType = Array.isArray(firstNonString)
      ? "array"
      : typeof firstNonString;
    throw new Error(
      `findDistinctByEditDistance requires string values, but '${responseColumn}' returned ${nonStringType}.`,
    );
  }

  return stringValues;
}

export async function findDistinctByEditDistance(
  db: Kysely<any>,
  query: Query,
  ctx: OperationContext,
) {
  assert(query.operation === "findDistinctByEditDistance", "Invalid operation");
  const { table, whereAndArray, operationParameters } = query;

  const { schema, dialect } = ctx;
  const dbForQuery = getSchemaBoundDb(db, schema, dialect);

  // Handle qualified column name (table.column format)
  const columnParts = operationParameters.column.split(".");
  const columnName =
    columnParts.length === 2 ? columnParts[1]! : operationParameters.column;

  assertColumnSupportsEditDistance(ctx, table, columnName);

  const distinctColumn = getColumnFromTable({
    columnName,
    tableName: table,
    schema,
    dialect,
  });

  // Build query with schema-qualified table name
  const { source: baseSource } = resolveBaseSource({
    tableName: table,
    tableSchema: query.tableSchema ?? null,
    schema,
    dialect,
  });
  let dbQuery = dbForQuery
    .selectFrom(baseSource as any)
    .select(distinctColumn.as(operationParameters.column))
    .distinct();

  dbQuery = applyLimit(dbQuery, 5000, dialect);

  // Add where conditions
  const whereCondition = buildWhereConditions(
    whereAndArray,
    table,
    schema,
    dialect,
    query.tableConditions,
  );
  if (whereCondition) {
    dbQuery = dbQuery.where(whereCondition);
  }

  const { rows: response, compiled } = await executeWithLogging(dbQuery, {
    operation: "findDistinctByEditDistance",
  });
  if (response.length > 4999) {
    throw new Error("Find Distinct limit hit at 5000");
  }

  const maxResults = 490;
  const reduced = filterStringByEditDistance(
    extractStringValuesForEditDistance(response, operationParameters.column),
    operationParameters.compareString,
    maxResults,
  );

  return {
    query: { sql: compiled.sql, params: compiled.parameters },
    data: reduced,
  };
}
