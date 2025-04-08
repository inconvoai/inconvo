import { type Query } from "~/types/querySchema";
import { parsePrismaWhere } from "~/operations/utils/prismaToDrizzleWhereConditions";
import { count, sql } from "drizzle-orm";
import { loadDrizzleSchema } from "~/util/loadDrizzleSchema";
import assert from "assert";
import { env } from "~/env";

type TemporalComponent = "Day" | "Month";

export async function countByTemporalComponent(db: any, query: Query) {
  assert(
    query.operation === "countByTemporalComponent",
    "Invalid inconvo operation"
  );
  const { table, whereAndArray, operationParameters, computedColumns } = query;

  const drizzleSchema = await loadDrizzleSchema();
  const dateColumn = drizzleSchema[table][operationParameters.dateColumn];

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

  const dbQuery = db
    .select({
      temporal_component: temporalExpression.as("temporal_component"),
      count: count(),
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
