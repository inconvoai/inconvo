import { type Query } from "~/types/querySchema";
import { avg, min, sql, sum, max, count, SQL } from "drizzle-orm";
import { parsePrismaWhere } from "~/operations/utils/prismaToDrizzleWhereConditions";
import { loadDrizzleSchema } from "~/util/loadDrizzleSchema";
import { getColumnFromTable } from "../utils/getColumnFromTable";
import { buildJsonObjectSelect } from "../utils/jsonBuilderHelpers";
import assert from "assert";

export async function aggregate(db: any, query: Query) {
  assert(query.operation === "aggregate", "Invalid inconvo operation");
  const {
    table: tableName,
    whereAndArray,
    operationParameters,
    computedColumns,
  } = query;

  const drizzleSchema = await loadDrizzleSchema();

  const createAggregateFields = (
    columns: string[] | undefined,
    aggregateFn: (column: SQL<any>) => SQL<any>
  ): [string, SQL<any>][] | undefined => {
    return columns?.map((columnName) => {
      const column = getColumnFromTable({
        columnName,
        tableName,
        drizzleSchema,
        computedColumns,
      });
      return [columnName, aggregateFn(column)];
    });
  };

  const selectFields: Record<string, any> = {};

  const avgFields = createAggregateFields(
    operationParameters.avg ?? undefined,
    (column) => avg(column)
  );
  const sumFields = createAggregateFields(
    operationParameters.sum ?? undefined,
    (column) => sum(column)
  );
  const minFields = createAggregateFields(
    operationParameters.min ?? undefined,
    (column) => min(column)
  );
  const maxFields = createAggregateFields(
    operationParameters.max ?? undefined,
    (column) => max(column)
  );
  const countFields = createAggregateFields(
    operationParameters.count ?? undefined,
    (column) => count(column)
  );
  const medianFields = createAggregateFields(
    operationParameters.median ?? undefined,
    (column) =>
      sql<number>`cast(percentile_cont(0.5) within group (order by ${column}) as Numeric)`.mapWith(
        Number
      )
  );

  if (avgFields) {
    selectFields["_avg"] = buildJsonObjectSelect(avgFields);
  }
  if (sumFields) {
    selectFields["_sum"] = buildJsonObjectSelect(sumFields);
  }
  if (minFields) {
    selectFields["_min"] = buildJsonObjectSelect(minFields);
  }
  if (maxFields) {
    selectFields["_max"] = buildJsonObjectSelect(maxFields);
  }
  if (countFields) {
    selectFields["_count"] = buildJsonObjectSelect(countFields);
  }
  if (medianFields) {
    selectFields["_median"] = buildJsonObjectSelect(medianFields);
  }

  const response = await db
    .select(selectFields)
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

  return response[0];
}
