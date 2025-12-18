import { Kysely } from "kysely";
import { Query } from "~/types/querySchema";
import { buildWhereConditions } from "~/operations/utils/whereConditionBuilder";
import { getAugmentedSchema } from "~/util/augmentedSchemaCache";
import { getColumnFromTable } from "~/operations/utils/computedColumns";
import { applyLimit } from "~/operations/utils/queryHelpers";
import { getSchemaBoundDb } from "~/operations/utils/schemaHelpers";
import assert from "assert";

export async function findDistinct(db: Kysely<any>, query: Query) {
  assert(query.operation === "findDistinct", "Invalid operation");
  const { table, whereAndArray, operationParameters } = query;

  const schema = await getAugmentedSchema();
  const dbForQuery = getSchemaBoundDb(db, schema);

  const distinctColumn = getColumnFromTable({
    columnName: operationParameters.column,
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

  const response = await dbQuery.execute();
  if (response.length >= limit) {
    throw new Error(`Find Distinct limit hit at ${limit}`);
  }

  const compiled = dbQuery.compile();

  return {
    query: { sql: compiled.sql, params: compiled.parameters },
    data: response,
  };
}
