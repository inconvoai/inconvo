import { type PrismaClient } from "@prisma/client";
import { type Query } from "~/types/querySchema";
import assert from "assert";
import { getTableColumns, sql, WithSubquery } from "drizzle-orm";
import { drizzle } from "drizzle-orm/prisma/pg/driver";
import * as drizzleTables from "../../../drizzle/schema";
import { parsePrismaWhere } from "~/util/prismaToDrizzleWhereConditions";

const tables: Record<string, any> = drizzleTables;

export async function findDistinctJson(prisma: PrismaClient, query: Query) {
  assert(query.operation === "findDistinct", "Invalid inconvo operation");
  const { table, whereAndArray, operationParameters, jsonColumnSchema } = query;

  const db = prisma.$extends(drizzle()).$drizzle;
  const drizzleWhere = parsePrismaWhere(tables[table], table, whereAndArray);

  const jsonSchemaForTable = jsonColumnSchema?.find(
    (jsonCol) => jsonCol.tableName === table
  );
  const jsonCols = jsonSchemaForTable?.jsonSchema.map((col) => col.name) || [];
  const jsonColumnName = jsonSchemaForTable?.jsonColumnName;
  const tableAlias = db.$with(`${table}Alias`).as(
    db
      .select({
        ...getTableColumns(tables[table]),
        ...jsonCols.reduce((acc: Record<string, unknown>, col) => {
          acc[col] = sql
            .raw(
              `cast((${jsonColumnName}->>'${col}') as ${
                jsonSchemaForTable?.jsonSchema.find((jCol) => jCol.name === col)
                  ? "Text"
                  : "Numeric"
              })`
            )
            .as(col);
          return acc;
        }, {}),
      })
      .from(tables[table])
      .where(drizzleWhere)
  );

  const response = await db
    .with(tableAlias)
    .selectDistinct({
      [operationParameters.column]: tableAlias[operationParameters.column],
    })
    .from(tableAlias);

  return response;
}
