import { Kysely } from "kysely";
import type { Query } from "../../types/querySchema";
import { buildWhereConditions } from "../utils/whereConditionBuilder";
import { getColumnFromTable } from "../utils/computedColumns";
import { applyLimit } from "../utils/queryHelpers";
import { getSchemaBoundDb } from "../utils/schemaHelpers";
import { getTableIdentifier } from "../utils/tableIdentifier";
import { executeWithLogging } from "../utils/executeWithLogging";
import type { OperationContext } from "../types";
import assert from "assert";

export async function findDistinct(
  db: Kysely<any>,
  query: Query,
  ctx: OperationContext,
) {
  assert(query.operation === "findDistinct", "Invalid operation");
  const { table, whereAndArray, operationParameters } = query;

  const { schema, dialect } = ctx;
  const dbForQuery = getSchemaBoundDb(db, schema, dialect);

  // Handle qualified column name (table.column format)
  const columnParts = operationParameters.column.split(".");
  const columnName =
    columnParts.length === 2 ? columnParts[1]! : operationParameters.column;

  const distinctColumn = getColumnFromTable({
    columnName,
    tableName: table,
    schema,
    dialect,
  });

  // Build query with schema-qualified table name
  const tableId = getTableIdentifier(table, query.tableSchema, dialect);
  let dbQuery = dbForQuery
    .selectFrom(tableId)
    .select(distinctColumn.as(operationParameters.column))
    .distinct();

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

  // Apply limit - hardcoded to 500
  const limit = 500;
  dbQuery = applyLimit(dbQuery, limit, dialect);

  const { rows: response, compiled } = await executeWithLogging(dbQuery, {
    operation: "findDistinct",
  });
  if (response.length >= limit) {
    throw new Error(`Find Distinct limit hit at ${limit}`);
  }

  return {
    query: { sql: compiled.sql, params: compiled.parameters },
    data: response,
  };
}
