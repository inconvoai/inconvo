import { type Query } from "~/types/querySchema";
import { parsePrismaWhere } from "~/operations/utils/prismaToDrizzleWhereConditions";
import { count, sql, avg, min, max, sum, asc, desc } from "drizzle-orm";
import { loadDrizzleSchema } from "~/util/loadDrizzleSchema";
import { env } from "~/env";
import assert from "assert";
import { getColumnFromTable } from "../utils/getColumnFromTable";
import { buildJsonObjectSelect } from "../utils/jsonBuilderHelpers";
import { createAggregationFields } from "../utils/createAggregationFields";

export async function groupByDateInterval(db: any, query: Query) {
  assert(
    query.operation === "groupByDateInterval",
    "Invalid inconvo operation"
  );

  const { table, whereAndArray, operationParameters, computedColumns } = query;
  const { interval } = operationParameters;

  const drizzleSchema = await loadDrizzleSchema();
  const dateColumn = drizzleSchema[table][operationParameters.dateColumn];

  assert(
    dateColumn,
    `Date Column ${operationParameters.dateColumn} not found in table ${table}`
  );

  let intervalExpression;
  if (env.DATABASE_DIALECT === "mysql") {
    switch (interval) {
      case "day":
        intervalExpression = sql`DATE_FORMAT(${dateColumn}, '%Y-%m-%d')`;
        break;
      case "week":
        intervalExpression = sql`YEARWEEK(${dateColumn})`;
        break;
      case "month":
        intervalExpression = sql`DATE_FORMAT(${dateColumn}, '%Y-%m')`;
        break;
      case "year":
        intervalExpression = sql`YEAR(${dateColumn})`;
        break;
    }
  } else if (env.DATABASE_DIALECT === "postgresql") {
    switch (interval) {
      case "day":
        intervalExpression = sql`to_char(${dateColumn}::date, 'YYYY-MM-DD')`;
        break;
      case "week":
        intervalExpression = sql`EXTRACT(YEAR FROM ${dateColumn}) || '-' || EXTRACT(WEEK FROM ${dateColumn})`;
        break;
      case "month":
        intervalExpression = sql`to_char(${dateColumn}::date, 'YYYY-MM')`;
        break;
      case "year":
        intervalExpression = sql`to_char(${dateColumn}::date, 'YYYY')`;
        break;
    }
  } else {
    throw new Error(
      "Unsupported database provider. URL must start with 'mysql' or 'postgres'"
    );
  }

  const countJsonFields = createAggregationFields(
    operationParameters.count?.map((col) => `${table}.${col}`),
    count,
    drizzleSchema,
    computedColumns
  );

  const minJsonFields = createAggregationFields(
    operationParameters.min?.map((col) => `${table}.${col}`),
    min,
    drizzleSchema,
    computedColumns
  );

  const maxJsonFields = createAggregationFields(
    operationParameters.max?.map((col) => `${table}.${col}`),
    max,
    drizzleSchema,
    computedColumns
  );

  const sumJsonFields = createAggregationFields(
    operationParameters.sum?.map((col) => `${table}.${col}`),
    sum,
    drizzleSchema,
    computedColumns
  );

  const avgJsonFields = createAggregationFields(
    operationParameters.avg?.map((col) => `${table}.${col}`),
    avg,
    drizzleSchema,
    computedColumns
  );

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

  const dbQuery = db
    .select({
      date_interval: intervalExpression.as("date_interval"),
      ...selectFields,
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
    .groupBy(intervalExpression)
    .orderBy(() => {
      if (operationParameters.orderBy) {
        if (operationParameters.orderBy === "chronological") {
          return asc(intervalExpression);
        } else if (operationParameters.orderBy === "reverseChronological") {
          return desc(intervalExpression);
        } else if (operationParameters.orderBy.function) {
          const direction =
            operationParameters.orderBy.direction === "asc" ? asc : desc;
          const column = getColumnFromTable({
            columnName: operationParameters.orderBy.column,
            tableName: table,
            drizzleSchema,
            computedColumns,
          });
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
          }
        }
      }
    });

  if (operationParameters.limit) {
    dbQuery.limit(operationParameters.limit);
  }

  const results = await dbQuery;

  return { query: dbQuery.toSQL(), data: results };
}
