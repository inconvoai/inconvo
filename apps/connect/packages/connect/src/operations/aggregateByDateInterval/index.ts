import { type Query } from "~/types/querySchema";
import { parsePrismaWhere } from "~/operations/utils/prismaToDrizzleWhereConditions";
import { count, sql, avg, min, max, sum } from "drizzle-orm";
import { loadDrizzleSchema } from "~/util/loadDrizzleSchema";
import { env } from "~/env";
import assert from "assert";
import { getColumnFromTable } from "../utils/getColumnFromTable";

export async function aggregateByDateInterval(db: any, query: Query) {
  assert(
    query.operation === "aggregateByDateInterval",
    "Invalid inconvo operation"
  );

  const { table, whereAndArray, operationParameters, computedColumns } = query;
  const { interval, aggregationType } = operationParameters;

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

  const aggregateColumn = getColumnFromTable({
    columnName: operationParameters.aggregateColumn,
    tableName: table,
    drizzleSchema,
    computedColumns,
  });

  let aggregationFunction;
  switch (aggregationType) {
    case "count":
      aggregationFunction = count();
      break;
    case "min":
      aggregationFunction = min(aggregateColumn);
      break;
    case "max":
      aggregationFunction = max(aggregateColumn);
      break;
    case "avg":
      aggregationFunction = avg(aggregateColumn);
      break;
    case "sum":
      aggregationFunction = sum(aggregateColumn);
      break;
  }

  const dbQuery = db
    .select({
      date_interval: intervalExpression.as("date_interval"),
      aggregation_value: aggregationFunction.as("aggregation_value"),
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
    .orderBy(intervalExpression);

  const results = await dbQuery;

  const resultsByInterval: Record<string, Record<string, number>> = {};
  results.forEach((row: any) => {
    resultsByInterval[row.date_interval] = {
      [aggregationType]: row.aggregation_value,
    };
  });

  return { query: dbQuery.toSQL(), data: resultsByInterval };
}
