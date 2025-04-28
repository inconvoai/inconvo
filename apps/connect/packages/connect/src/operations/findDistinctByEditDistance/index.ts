import { type Query } from "~/types/querySchema";
import { parsePrismaWhere } from "~/operations/utils/prismaToDrizzleWhereConditions";
import { loadDrizzleSchema } from "~/util/loadDrizzleSchema";
import { getColumnFromTable } from "../utils/getColumnFromTable";
import * as levenshtein from "fast-levenshtein";
import assert from "assert";

function filterStringByEditDistance(
  strings: string[],
  targetString: string,
  maxResults: number
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

export async function findDistinctByEditDistance(db: any, query: Query) {
  assert(
    query.operation === "findDistinctByEditDistance",
    "Invalid inconvo operation"
  );
  const { table, whereAndArray, operationParameters, computedColumns } = query;

  const drizzleSchema = await loadDrizzleSchema();

  const distinctColumn = getColumnFromTable({
    columnName: operationParameters.column,
    tableName: table,
    drizzleSchema,
    computedColumns,
  });

  const dbQuery = db

    .selectDistinct({
      [operationParameters.column]: distinctColumn,
    })
    .from(drizzleSchema[table])
    .where((columns: Record<string, unknown>) =>
      parsePrismaWhere({
        drizzleSchema,
        tableName: table,
        where: whereAndArray,
        columns,
        computedColumns: computedColumns,
      })
    )
    .limit(5000);

  const response = await dbQuery;
  if (response.length > 4999) {
    throw new Error("Find Distinct limit hit at 5000");
  }

  const maxResults = 490;
  const reduced = filterStringByEditDistance(
    response
      .map((value: Record<string, string>) => value[operationParameters.column])
      .filter((value: string) => value !== null && value !== undefined),
    operationParameters.compareString,
    maxResults
  );

  return { query: dbQuery.toSQL(), data: reduced };
}
