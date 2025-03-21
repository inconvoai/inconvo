import { type Query } from "~/types/querySchema";
import { parsePrismaWhere } from "~/util/prismaToDrizzleWhereConditions";
import { count, sql, avg, min, max, sum } from "drizzle-orm";
import { loadDrizzleSchema } from "~/util/loadDrizzleSchema";
import { db } from "~/dbConnection";
import assert from "assert";

// Ensure database dialect is detected correctly
const dbDialect = "postgres";

export async function aggregateByDateInterval(query: Query) {
  assert(
    query.operation === "aggregateByDateInterval",
    "Invalid inconvo operation"
  );

  const { table, whereAndArray, operationParameters } = query;
  const { interval, aggregationType } = operationParameters;

  const tables = await loadDrizzleSchema();
  const dateColumn = tables[table][operationParameters.dateColumn];

  if (!dateColumn) {
    throw new Error(
      `Column ${operationParameters.dateColumn} not found in table ${table}`
    );
  }

  let intervalExpression;
  // @ts-expect-error
  if (dbDialect === "mysql") {
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
  } else if (dbDialect === "postgres") {
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

  const drizzleWhere = parsePrismaWhere(tables[table], table, whereAndArray);

  const aggregateColumn = tables[table][operationParameters.aggregateColumn];

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

  let dbQuery = db
    .select({
      date_interval: intervalExpression.as("date_interval"),
      aggregation_value: aggregationFunction.as("aggregation_value"),
    })
    .from(tables[table])
    .where(drizzleWhere)
    .groupBy(intervalExpression)
    .orderBy(intervalExpression);

  const results = await dbQuery;

  const resultsByInterval: Record<string, Record<string, number>> = {};
  results.forEach((row: any) => {
    resultsByInterval[row.date_interval] = {
      [aggregationType]: row.aggregation_value,
    };
  });

  return resultsByInterval;
}
