import { type PrismaClient } from "@prisma/client";
import { type Query } from "~/types/querySchema";
import assert from "assert";
import { drizzle } from "drizzle-orm/prisma/pg";
import { sql } from "drizzle-orm";
import { parsePrismaWhere } from "~/util/prismaToDrizzleWhereConditions";
import { loadDrizzleTables } from "../utils";

export async function aggregateJson(prisma: PrismaClient, query: Query) {
  assert(query.operation === "aggregate", "Invalid inconvo operation");
  const { table, whereAndArray, operationParameters, jsonColumnSchema } = query;

  const tables = await loadDrizzleTables();
  const db = prisma.$extends(drizzle()).$drizzle;
  const drizzleWhere = parsePrismaWhere(tables[table], table, whereAndArray);

  // Extract all unique columns from all aggregate functions
  const allColumnNames = new Set<string>();
  if (operationParameters.min)
    operationParameters.min.forEach((col) => allColumnNames.add(col));
  if (operationParameters.max)
    operationParameters.max.forEach((col) => allColumnNames.add(col));
  if (operationParameters.avg)
    operationParameters.avg.forEach((col) => allColumnNames.add(col));
  if (operationParameters.sum)
    operationParameters.sum.forEach((col) => allColumnNames.add(col));
  if (operationParameters.count)
    operationParameters.count.forEach((col) => allColumnNames.add(col));
  if (operationParameters.median)
    operationParameters.median.forEach((col) => allColumnNames.add(col));

  const uniqueColumnNames = Array.from(allColumnNames);

  const tableJsonColumnSchema = jsonColumnSchema?.find(
    (x) => x.tableName === table
  );

  const selectQuery: Record<string, any> = {};
  for (const name of uniqueColumnNames) {
    const jsonData = tableJsonColumnSchema?.jsonSchema.find(
      (x) => x.name === name
    );
    if (tables[table][name]) {
      selectQuery[name] = tables[table][name];
    } else if (jsonData) {
      const castType = jsonData.type === "Number" ? "Numeric" : "Text";
      selectQuery[name] = sql
        .raw(
          `cast((${tableJsonColumnSchema?.jsonColumnName}->>'${name}') as ${castType})`
        )
        .as(name);
    }
  }

  const tmpTable = db
    .$with("tmpTable")
    .as(db.select(selectQuery).from(tables[table]).where(drizzleWhere));

  const aggregateSelect: Record<string, any> = {};

  // Only apply aggregate functions for requested columns
  if (operationParameters.avg) {
    operationParameters.avg.forEach((column) => {
      aggregateSelect[`avg_${column}`] =
        sql<number>`cast(avg(${tmpTable[column]}) as Numeric)`
          .mapWith(Number)
          .as(`avg_${column}`);
    });
  }

  if (operationParameters.sum) {
    operationParameters.sum.forEach((column) => {
      aggregateSelect[`sum_${column}`] =
        sql<number>`cast(sum(${tmpTable[column]}) as Numeric)`
          .mapWith(Number)
          .as(`sum_${column}`);
    });
  }

  if (operationParameters.min) {
    operationParameters.min.forEach((column) => {
      aggregateSelect[`min_${column}`] =
        sql<number>`cast(min(${tmpTable[column]}) as Numeric)`
          .mapWith(Number)
          .as(`min_${column}`);
    });
  }

  if (operationParameters.max) {
    operationParameters.max.forEach((column) => {
      aggregateSelect[`max_${column}`] =
        sql<number>`cast(max(${tmpTable[column]}) as Numeric)`
          .mapWith(Number)
          .as(`max_${column}`);
    });
  }

  if (operationParameters.median) {
    operationParameters.median.forEach((column) => {
      aggregateSelect[`median_${column}`] =
        sql<number>`cast(percentile_cont(0.5) within group (order by ${tmpTable[column]}) as Numeric)`
          .mapWith(Number)
          .as(`median_${column}`);
    });
  }

  if (operationParameters.count) {
    operationParameters.count.forEach((column) => {
      aggregateSelect[`count_${column}`] =
        sql<number>`cast(count(${tmpTable[column]}) as Int)`.as(
          `count_${column}`
        );
    });
  }

  const response = (await db
    .with(tmpTable)
    .select(aggregateSelect)
    .from(tmpTable)) as any;

  const formattedResponse = response[0]
    ? Object.keys(response[0]).reduce(
        (
          acc: {
            _avg?: { [key: string]: any };
            _sum?: { [key: string]: any };
            _min?: { [key: string]: any };
            _max?: { [key: string]: any };
            _median?: { [key: string]: any };
            _count?: { [key: string]: any };
          },
          key
        ) => {
          if (key.startsWith("avg_")) {
            acc._avg = acc._avg || {};
            acc._avg[key.replace("avg_", "")] = response[0][key];
          }
          if (key.startsWith("sum_")) {
            acc._sum = acc._sum || {};
            acc._sum[key.replace("sum_", "")] = response[0][key];
          }
          if (key.startsWith("min_")) {
            acc._min = acc._min || {};
            acc._min[key.replace("min_", "")] = response[0][key];
          }
          if (key.startsWith("max_")) {
            acc._max = acc._max || {};
            acc._max[key.replace("max_", "")] = response[0][key];
          }
          if (key.startsWith("median_")) {
            acc._median = acc._median || {};
            acc._median[key.replace("median_", "")] = response[0][key];
          }
          if (key.startsWith("count_")) {
            acc._count = acc._count || {};
            acc._count[key.replace("count_", "")] = response[0][key];
          }
          return acc;
        },
        {}
      )
    : {};

  return formattedResponse;
}
