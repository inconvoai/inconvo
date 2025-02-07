import { type PrismaClient } from "@prisma/client";
import { type Query } from "~/types/querySchema";
import { drizzle } from "drizzle-orm/prisma/pg";
import { count, desc, getTableColumns, max, min, sql, sum } from "drizzle-orm";
import { parsePrismaWhere } from "~/util/prismaToDrizzleWhereConditions";
import assert from "assert";
import * as drizzleTables from "~/../drizzle/schema";

const tables: Record<string, any> = drizzleTables;

export async function groupByJson(prisma: PrismaClient, query: Query) {
  assert(query.operation === "groupBy", "Invalid inconvo operation");
  const { table, whereAndArray, operationParameters, jsonColumnSchema } = query;

  const db = prisma.$extends(drizzle()).$drizzle;

  const jsonSchemaForTable = jsonColumnSchema?.find(
    (jsonCol) => jsonCol.tableName === table
  );
  const jsonCols = jsonSchemaForTable?.jsonSchema.map((col) => col.name) || [];
  const jsonColumnName = jsonSchemaForTable?.jsonColumnName;
  const drizzleWhere = parsePrismaWhere(tables[table], table, whereAndArray);

  const tableAlias = db.$with(`${table}Alias`).as(
    db
      .select({
        ...getTableColumns(tables[table]),
        ...jsonCols.reduce((acc: Record<string, unknown>, col) => {
          acc[col] = sql
            .raw(
              `cast((${jsonColumnName}->>'${col}') as ${
                jsonSchemaForTable?.jsonSchema.find((jCol) => jCol.name === col)
                  ?.type === "String"
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

  const countJsonFields = operationParameters.count?.columns.map(
    (col) => sql`${col}::text, cast(${count(tableAlias[col])} as int)`
  );
  const minJsonFields = operationParameters.min?.columns.map(
    (col) => sql`${col}::text, cast(${min(tableAlias[col])} as Numeric)`
  );
  const maxJsonFields = operationParameters.max?.columns.map(
    (col) => sql`${col}::text, cast(${max(tableAlias[col])} as Numeric)`
  );
  const sumJsonFields = operationParameters.sum?.columns.map(
    (col) => sql`${col}::text, cast(${sum(tableAlias[col])} as Numeric)`
  );

  const selectFields: Record<string, any> = {};

  if (countJsonFields) {
    selectFields.count = sql`json_build_object${countJsonFields}`.as("_count");
  }
  if (minJsonFields) {
    selectFields.min = sql`json_build_object${minJsonFields}`.as("_min");
  }
  if (maxJsonFields) {
    selectFields.max = sql`json_build_object${maxJsonFields}`.as("_max");
  }
  if (sumJsonFields) {
    selectFields.sum = sql`json_build_object${sumJsonFields}`.as("_sum");
  }

  // TODO: Add support for groupBy after join
  const response = await db
    .with(tableAlias)
    .select({
      [operationParameters.groupBy[0].column]:
        tableAlias[operationParameters.groupBy[0].column],
      ...selectFields,
    })
    .from(tableAlias)
    .groupBy(tableAlias[operationParameters.groupBy[0].column])
    .orderBy(desc(count(tableAlias["id"])));

  return response;
}
