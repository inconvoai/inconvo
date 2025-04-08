import { type Query } from "~/types/querySchema";
import { avg, min, sql, sum, max, count, SQL } from "drizzle-orm";
import { parsePrismaWhere } from "~/operations/utils/prismaToDrizzleWhereConditions";
import { loadDrizzleSchema } from "~/util/loadDrizzleSchema";
import { getColumnFromTable } from "../utils/getColumnFromTable";
import assert from "assert";

type AggregateTypes = "avg" | "sum" | "min" | "max" | "median" | "count";
type AggregateResult = {
  _avg?: Record<string, number>;
  _sum?: Record<string, number>;
  _min?: Record<string, number>;
  _max?: Record<string, number>;
  _median?: Record<string, number>;
  _count?: Record<string, number>;
};

export async function aggregate(db: any, query: Query) {
  assert(query.operation === "aggregate", "Invalid inconvo operation");
  const {
    table: tableName,
    whereAndArray,
    operationParameters,
    computedColumns,
  } = query;

  const drizzleSchema = await loadDrizzleSchema();

  const aggregateFunctions: Record<AggregateTypes, (column: string) => any> = {
    avg: (column) =>
      avg(
        getColumnFromTable({
          columnName: column,
          tableName,
          drizzleSchema,
          computedColumns,
        })
      ).as(`avg_${column}`),
    sum: (column) =>
      sum(
        getColumnFromTable({
          columnName: column,
          tableName,
          drizzleSchema,
          computedColumns,
        })
      ).as(`sum_${column}`),
    min: (column) =>
      min(
        getColumnFromTable({
          columnName: column,
          tableName,
          drizzleSchema,
          computedColumns,
        })
      ).as(`min_${column}`),
    max: (column) =>
      max(
        getColumnFromTable({
          columnName: column,
          tableName,
          drizzleSchema,
          computedColumns,
        })
      ).as(`max_${column}`),
    count: (column) =>
      count(
        getColumnFromTable({
          columnName: column,
          tableName,
          drizzleSchema,
          computedColumns,
        })
      ).as(`count_${column}`),
    median: (column) =>
      sql<number>`cast(percentile_cont(0.5) within group (order by ${getColumnFromTable(
        {
          columnName: column,
          tableName,
          drizzleSchema,
          computedColumns,
        }
      )}) as Numeric)`
        .mapWith(Number)
        .as(`median_${column}`),
  };

  const aggregateSelect: Record<string, any> = {};
  Object.entries(operationParameters)
    .filter(([_, value]) => Array.isArray(value) && value !== null)
    .forEach(([type, columns]) => {
      if (type in aggregateFunctions && Array.isArray(columns)) {
        columns.forEach((column) => {
          const aggFunction = aggregateFunctions[type as AggregateTypes];
          aggregateSelect[`${type}_${column}`] = aggFunction(column);
        });
      }
    });

  const response = await db
    .select(aggregateSelect)
    .from(drizzleSchema[tableName])
    .where((columns: Record<string, SQL>) =>
      parsePrismaWhere({
        drizzleSchema,
        columns,
        tableName,
        where: whereAndArray,
        computedColumns,
      })
    );

  return Object.keys(response[0]).reduce((acc: AggregateResult, key) => {
    const aggregateType = Object.keys(aggregateFunctions).find((type) =>
      key.startsWith(`${type}_`)
    );
    if (aggregateType) {
      const column = key.substring(aggregateType.length + 1); // +1 for the underscore
      const resultKey = `_${aggregateType}` as keyof AggregateResult;
      if (!acc[resultKey]) {
        acc[resultKey] = {};
      }
      (acc[resultKey] as Record<string, number>)[column] = response[0][key];
    }
    return acc;
  }, {});
}
