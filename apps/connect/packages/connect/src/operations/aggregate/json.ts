import { type PrismaClient } from "@prisma/client";
import { type Query } from "~/types/querySchema";
import assert from "assert";
import { drizzle } from "drizzle-orm/prisma/pg";
import { sql } from "drizzle-orm";
import * as drizzleTables from "../../../drizzle/schema";
import { parsePrismaWhere } from "~/util/prismaToDrizzleWhereConditions";

const tables: Record<string, any> = drizzleTables;

export async function aggregateJson(prisma: PrismaClient, query: Query) {
  assert(query.operation === "aggregate", "Invalid inconvo operation");
  const { table, whereAndArray, operationParameters, jsonColumnSchema } = query;

  const prismaDrizzle = prisma.$extends(drizzle()).$drizzle;
  const drizzleWhere = parsePrismaWhere(tables[table], table, whereAndArray);

  const columnNames = operationParameters.columns;

  const tableJsonColumnSchema = jsonColumnSchema?.find(
    (x) => x.tableName === table
  );

  const selectQuery: Record<string, any> = {};
  for (const name of columnNames) {
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

  const tmpTable = prismaDrizzle
    .$with("tmpTable")
    .as(
      prismaDrizzle.select(selectQuery).from(tables[table]).where(drizzleWhere)
    );

  const aggregateSelect = columnNames.reduce((acc, column) => {
    const colSelect = {
      [`avg_${column}`]: sql<number>`cast(avg(${tmpTable[column]}) as Numeric)`
        .mapWith(Number)
        .as(`avg_${column}`),
      [`sum_${column}`]: sql<number>`cast(sum(${tmpTable[column]}) as Numeric)`
        .mapWith(Number)
        .as(`sum_${column}`),
      [`min_${column}`]: sql<number>`cast(min(${tmpTable[column]}) as Numeric)`
        .mapWith(Number)
        .as(`min_${column}`),
      [`max_${column}`]: sql<number>`cast(max(${tmpTable[column]}) as Numeric)`
        .mapWith(Number)
        .as(`max_${column}`),
      [`count_${column}`]:
        sql<number>`cast(count(${tmpTable[column]}) as Int)`.as(
          `count_${column}`
        ),
    };
    return { ...acc, ...colSelect };
  }, {});

  const response = (await prismaDrizzle
    .with(tmpTable)
    .select(aggregateSelect)
    .from(tmpTable)) as any;

  const formattedResponse = response[0]
    ? Object.keys(response[0]).reduce(
        (
          acc: {
            _avg: { [key: string]: any };
            _sum: { [key: string]: any };
            _min: { [key: string]: any };
            _max: { [key: string]: any };
            _count: { [key: string]: any };
          },
          key
        ) => {
          if (key.startsWith("avg_"))
            acc._avg[key.replace("avg_", "")] = response[0][key];
          if (key.startsWith("sum_"))
            acc._sum[key.replace("sum_", "")] = response[0][key];
          if (key.startsWith("min_"))
            acc._min[key.replace("min_", "")] = response[0][key];
          if (key.startsWith("max_"))
            acc._max[key.replace("max_", "")] = response[0][key];
          if (key.startsWith("count_"))
            acc._count[key.replace("count_", "")] = response[0][key];
          return acc;
        },
        { _avg: {}, _sum: {}, _min: {}, _max: {}, _count: {} }
      )
    : {};

  return formattedResponse;
}
