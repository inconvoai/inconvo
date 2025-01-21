import { type PrismaClient } from "@prisma/client";
import { type Query } from "~/types/querySchema";
import assert from "assert";
import { drizzle } from "drizzle-orm/prisma/pg";
import { sql } from "drizzle-orm";
import * as drizzleTables from "../../../drizzle/schema";
import { parsePrismaWhere } from "~/util/prismaToDrizzleWhereConditions";

const tables: Record<string, any> = drizzleTables;

export async function countJson(prisma: PrismaClient, query: Query) {
  assert(query.operation === "count", "Invalid inconvo operation");
  const { table, whereAndArray, operationParameters, jsonColumnSchema } = query;

  const columnNames = operationParameters.columns;

  const selectQuery: Record<string, any> = {};
  for (const name of columnNames) {
    const jsonData = jsonColumnSchema?.jsonSchema.find((x) => x.key === name);
    if (tables[table][name]) {
      selectQuery[name] = tables[table][name];
    } else if (jsonData) {
      const castType = jsonData.type === "number" ? "Numeric" : "Text";
      selectQuery[name] = sql
        .raw(
          `cast((${jsonColumnSchema?.nameOfJsonColumn}->>'${name}') as ${castType})`
        )
        .as(name);
    }
  }

  const prismaDrizzle = prisma.$extends(drizzle()).$drizzle;
  const drizzleWhere = parsePrismaWhere(tables[table], whereAndArray);
  const tmpTable = prismaDrizzle
    .$with("tmpTable")
    .as(
      prismaDrizzle.select(selectQuery).from(tables[table]).where(drizzleWhere)
    );

  const aggregateSelect = columnNames.reduce((acc, column) => {
    const colSelect = {
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
            _count: { [key: string]: any };
          },
          key
        ) => {
          if (key.startsWith("count_"))
            acc._count[key.replace("count_", "")] = response[0][key];
          return acc;
        },
        { _count: {} }
      )
    : {};

  return formattedResponse;
}
