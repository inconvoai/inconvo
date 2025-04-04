import { type Query } from "~/types/querySchema";
import { asc, avg, count, desc, eq, max, min, SQL, sum } from "drizzle-orm";
import { parsePrismaWhere } from "~/operations/utils/prismaToDrizzleWhereConditions";
import { findRelationsBetweenTables } from "~/operations/utils/findRelationsBetweenTables";
import { loadDrizzleSchema } from "~/util/loadDrizzleSchema";
import { buildJsonObjectSelect } from "../utils/jsonBuilderHelpers";
import assert from "assert";

export async function groupBy(db: any, query: Query) {
  assert(query.operation === "groupBy", "Invalid inconvo operation");
  const { table, whereAndArray, operationParameters, computedColumns } = query;

  const drizzleSchema = await loadDrizzleSchema();

  const countJsonFields: [string, SQL<number | null>][] | undefined =
    operationParameters.count?.columns.map((col) => [
      col,
      count(drizzleSchema[table][col]),
    ]);
  const minJsonFields: [string, SQL<number | null>][] | undefined =
    operationParameters.min?.columns.map((col) => [
      col,
      min(drizzleSchema[table][col]),
    ]);
  const maxJsonFields: [string, SQL<number | null>][] | undefined =
    operationParameters.max?.columns.map((col) => [
      col,
      max(drizzleSchema[table][col]),
    ]);
  const sumJsonFields: [string, SQL<string | null>][] | undefined =
    operationParameters.sum?.columns.map((col) => [
      col,
      sum(drizzleSchema[table][col]),
    ]);
  const avgJsonFields: [string, SQL<string | null>][] | undefined =
    operationParameters.avg?.columns.map((col) => [
      col,
      avg(drizzleSchema[table][col]),
    ]);

  const selectFields: Record<string, any> = {};

  if (countJsonFields) {
    selectFields["_count"] = buildJsonObjectSelect(countJsonFields);
  }
  if (minJsonFields) {
    selectFields["_min"] = buildJsonObjectSelect(minJsonFields);
  }
  if (maxJsonFields) {
    selectFields["_max"] = buildJsonObjectSelect(maxJsonFields);
  }
  if (sumJsonFields) {
    selectFields["_sum"] = buildJsonObjectSelect(sumJsonFields);
  }
  if (avgJsonFields) {
    selectFields["_avg"] = buildJsonObjectSelect(avgJsonFields);
  }

  const joinEntry = Object.entries(
    operationParameters.groupBy[0]?.join ?? {}
  )[0];
  const [joinTable, joinColumn] = joinEntry
    ? joinEntry
    : [undefined, undefined];

  const dbQuery = db
    .select({
      [operationParameters.groupBy[0].column]:
        drizzleSchema[table][operationParameters.groupBy[0].column],
      ...selectFields,
      ...(joinTable
        ? { [joinColumn]: drizzleSchema[joinTable][joinColumn] }
        : {}),
    })
    .from(drizzleSchema[table])
    .groupBy(
      drizzleSchema[table][operationParameters.groupBy[0].column],
      ...(joinTable ? [drizzleSchema[joinTable][joinColumn]] : [])
    )
    .where((columns: Record<string, unknown>) =>
      parsePrismaWhere({
        drizzleSchema,
        tableName: table,
        where: whereAndArray,
        columns,
        computedColumns: computedColumns,
      })
    )
    .orderBy(() => {
      const column = drizzleSchema[table][operationParameters.orderBy.column];
      const direction =
        operationParameters.orderBy.direction === "asc" ? asc : desc;

      switch (operationParameters.orderBy.function) {
        case "count":
          return direction(count(column));
        case "sum":
          return direction(sum(column));
        case "min":
          return direction(min(column));
        case "max":
          return direction(max(column));
        case "avg":
          return direction(avg(column));
        default:
          return direction(count(column)); // Fallback to count
      }
    })
    .limit(operationParameters.limit);

  if (joinTable) {
    const [currentTableKey, relatedTableKey] = findRelationsBetweenTables(
      table,
      joinTable,
      joinTable,
      drizzleSchema
    );
    dbQuery.leftJoin(
      drizzleSchema[joinTable],
      eq(
        drizzleSchema[table][currentTableKey],
        drizzleSchema[joinTable][relatedTableKey]
      )
    );
  }

  return dbQuery;
}
