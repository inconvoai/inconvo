import { type PrismaClient } from "@prisma/client";
import { type Query } from "~/types/querySchema";
import assert from "assert";
import { drizzle } from "drizzle-orm/prisma/pg";
import { sql } from "drizzle-orm";
import { parsePrismaWhere } from "~/util/prismaToDrizzleWhereConditions";
import { loadDrizzleTables } from "../utils";

export async function countJson(prisma: PrismaClient, query: Query) {
  assert(query.operation === "count", "Invalid inconvo operation");
  const { table, whereAndArray, operationParameters, jsonColumnSchema } = query;

  const tables = await loadDrizzleTables();
  const db = prisma.$extends(drizzle()).$drizzle;
  const drizzleWhere = parsePrismaWhere(tables[table], table, whereAndArray);

  const columnNames = operationParameters.columns;

  const jsonSchemaForTable = jsonColumnSchema?.find(
    (jsonCol) => jsonCol.tableName === table
  );

  const selectQuery: Record<string, any> = {};
  for (const name of columnNames) {
    const jsonData = jsonSchemaForTable?.jsonSchema.find(
      (x) => x.name === name
    );
    if (tables[table][name]) {
      selectQuery[name] = tables[table][name];
    } else if (jsonData) {
      const castType = jsonData.type === "number" ? "Numeric" : "Text";
      selectQuery[name] = sql
        .raw(
          `cast((${jsonSchemaForTable?.jsonColumnName}->>'${name}') as ${castType})`
        )
        .as(name);
    }
  }

  const tmpTable = db
    .$with("tmpTable")
    .as(db.select(selectQuery).from(tables[table]).where(drizzleWhere));

  const aggregateSelect = columnNames.reduce((acc, column) => {
    const colSelect = {
      [`count_${column}`]:
        sql<number>`cast(count(${tmpTable[column]}) as Int)`.as(
          `count_${column}`
        ),
    };
    return { ...acc, ...colSelect };
  }, {});

  const response = (await db
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
