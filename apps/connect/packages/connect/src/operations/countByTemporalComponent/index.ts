import { type Query } from "~/types/querySchema";
import { parsePrismaWhere } from "~/util/prismaToDrizzleWhereConditions";
import { count, sql } from "drizzle-orm";
import { loadDrizzleSchema } from "~/util/loadDrizzleSchema";
import { db } from "~/dbConnection";
import assert from "assert";
import { env } from "~/env";

type TemporalComponent = "Day" | "Month";

export async function countByTemporalComponent(query: Query) {
  assert(
    query.operation === "countByTemporalComponent",
    "Invalid inconvo operation"
  );
  const { table, whereAndArray, operationParameters, jsonColumnSchema } = query;

  const tables = await loadDrizzleSchema();
  const dateColumn = tables[table][operationParameters.dateColumn];

  if (!dateColumn) {
    throw new Error(
      `Column ${operationParameters.dateColumn} not found in table ${table}`
    );
  }

  let temporalExpression;
  if (env.DATABASE_DIALECT === "mysql") {
    switch (operationParameters.component) {
      case "Day":
        temporalExpression = sql`trim(DATE_FORMAT(${dateColumn}, '%W'))`;
        break;
      case "Month":
        temporalExpression = sql`trim(DATE_FORMAT(${dateColumn}, '%M'))`;
        break;
      default:
        throw new Error("Invalid temporal component. Must be 'Day' or 'Month'");
    }
  } else if (env.DATABASE_DIALECT === "postgresql") {
    switch (operationParameters.component) {
      case "Day":
        temporalExpression = sql`trim(to_char(${dateColumn}::date, 'Day'))`;
        break;
      case "Month":
        temporalExpression = sql`trim(to_char(${dateColumn}::date, 'Month'))`;
        break;
      default:
        throw new Error("Invalid temporal component. Must be 'Day' or 'Month'");
    }
  } else {
    throw new Error(
      "Unsupported database provider. URL must start with 'mysql' or 'postgres'"
    );
  }

  const drizzleWhere = parsePrismaWhere(tables[table], table, whereAndArray);

  let dbQuery = db
    .select({
      temporal_component: temporalExpression.as("temporal_component"),
      count: count(),
    })
    .from(tables[table])
    .where(drizzleWhere)
    .groupBy(temporalExpression);

  const results = await dbQuery;

  const countByComponent: Record<string, number> = {};

  const sortOrder = {
    Day: [
      "Monday",
      "Tuesday",
      "Wednesday",
      "Thursday",
      "Friday",
      "Saturday",
      "Sunday",
    ],
    Month: [
      "January",
      "February",
      "March",
      "April",
      "May",
      "June",
      "July",
      "August",
      "September",
      "October",
      "November",
      "December",
    ],
  };

  results.sort((a: any, b: any) => {
    const order = sortOrder[operationParameters.component as TemporalComponent];
    return (
      order.indexOf(a.temporal_component) - order.indexOf(b.temporal_component)
    );
  });

  results.forEach((row: any) => {
    countByComponent[row.temporal_component] = row.count;
  });

  return countByComponent;
}
