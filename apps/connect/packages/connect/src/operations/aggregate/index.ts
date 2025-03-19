import { type Query } from "~/types/querySchema";
import { sql } from "drizzle-orm";
import { parsePrismaWhere } from "~/util/prismaToDrizzleWhereConditions";
import { loadDrizzleTables } from "../utils";
import assert from "assert";
import { db } from "~/dbConnection";

export async function aggregate(query: Query) {
  assert(query.operation === "aggregate", "Invalid inconvo operation");
  const {
    table,
    operation,
    whereAndArray,
    operationParameters,
    computedColumns,
    jsonColumnSchema,
  } = query;

  const tables = await loadDrizzleTables();
  const dbTable = tables[table];

  const drizzleWhere = parsePrismaWhere(tables[table], table, whereAndArray);

  const aggregateSelect: Record<string, any> = {};

  if (operationParameters.avg) {
    operationParameters.avg.forEach((column) => {
      aggregateSelect[`avg_${column}`] =
        sql<number>`cast(avg(${dbTable[column]}) as Numeric)`
          .mapWith(Number)
          .as(`avg_${column}`);
    });
  }

  if (operationParameters.sum) {
    operationParameters.sum.forEach((column) => {
      aggregateSelect[`sum_${column}`] =
        sql<number>`cast(sum(${dbTable[column]}) as Numeric)`
          .mapWith(Number)
          .as(`sum_${column}`);
    });
  }

  if (operationParameters.min) {
    operationParameters.min.forEach((column) => {
      aggregateSelect[`min_${column}`] =
        sql<number>`cast(min(${dbTable[column]}) as Numeric)`
          .mapWith(Number)
          .as(`min_${column}`);
    });
  }

  if (operationParameters.max) {
    operationParameters.max.forEach((column) => {
      aggregateSelect[`max_${column}`] =
        sql<number>`cast(max(${dbTable[column]}) as Numeric)`
          .mapWith(Number)
          .as(`max_${column}`);
    });
  }

  if (operationParameters.median) {
    operationParameters.median.forEach((column) => {
      aggregateSelect[`median_${column}`] =
        sql<number>`cast(percentile_cont(0.5) within group (order by ${dbTable[column]}) as Numeric)`
          .mapWith(Number)
          .as(`median_${column}`);
    });
  }

  if (operationParameters.count) {
    operationParameters.count.forEach((column) => {
      aggregateSelect[`count_${column}`] =
        sql<number>`cast(count(${dbTable[column]}) as Int)`.as(
          `count_${column}`
        );
    });
  }

  const response = (await db
    .select(aggregateSelect)
    .from(tables[table])
    .where(drizzleWhere)) as any;

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
