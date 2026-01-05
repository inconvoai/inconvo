import { Kysely } from "kysely";
import type { Query } from "../../types/querySchema";
import { buildWhereConditions } from "../utils/whereConditionBuilder";
import { getAugmentedSchema } from "../../util/augmentedSchemaCache";
import { getColumnFromTable } from "../utils/computedColumns";
import { applyLimit } from "../utils/queryHelpers";
import { getSchemaBoundDb } from "../utils/schemaHelpers";
import { executeWithLogging } from "../utils/executeWithLogging";
import * as levenshtein from "fast-levenshtein";
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

export async function findDistinctByEditDistance(
  db: Kysely<any>,
  query: Query,
) {
  assert(query.operation === "findDistinctByEditDistance", "Invalid operation");
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

  dbQuery = applyLimit(dbQuery, 5000);

  // Add where conditions
  const whereCondition = buildWhereConditions(whereAndArray, table, schema);
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
    response
      .map(
        (value: Record<string, unknown>) => value[operationParameters.column],
      )
      .filter(
        (value: unknown): value is string =>
          value !== null && value !== undefined,
      ),
    operationParameters.compareString,
    maxResults,
  );

  return {
    query: { sql: compiled.sql, params: compiled.parameters },
    data: reduced,
  };
}
