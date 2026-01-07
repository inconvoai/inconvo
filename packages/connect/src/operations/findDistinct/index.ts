import { Kysely } from "kysely";
import type { Query } from "../../types/querySchema";
import { buildWhereConditions } from "../utils/whereConditionBuilder";
import { getAugmentedSchema } from "../../util/augmentedSchemaCache";
import { getColumnFromTable } from "../utils/computedColumns";
import { applyLimit } from "../utils/queryHelpers";
import { getSchemaBoundDb } from "../utils/schemaHelpers";
import { executeWithLogging } from "../utils/executeWithLogging";
import assert from "assert";

export async function findDistinct(db: Kysely<any>, query: Query) {
  assert(query.operation === "findDistinct", "Invalid operation");
  const { table, whereAndArray, operationParameters } = query;

  const schema = await getAugmentedSchema();
  const dbForQuery = getSchemaBoundDb(db, schema);

  // Handle qualified column name (table.column format)
  const columnParts = operationParameters.column.split(".");
  const columnName =
    columnParts.length === 2 ? columnParts[1]! : operationParameters.column;

  const distinctColumn = getColumnFromTable({
    columnName,
    tableName: table,
    schema,
  });

  let dbQuery = dbForQuery
    .selectFrom(table)
    .select(distinctColumn.as(operationParameters.column))
    .distinct();

  // Add where conditions
  const whereCondition = buildWhereConditions(whereAndArray, table, schema);
  if (whereCondition) {
    dbQuery = dbQuery.where(whereCondition);
  }

  // Apply limit - hardcoded to 500
  const limit = 500;
  dbQuery = applyLimit(dbQuery, limit);

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
