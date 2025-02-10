import { type PrismaClient } from "@prisma/client";
import { type Query } from "~/types/querySchema";
import { drizzle } from "drizzle-orm/prisma/pg";
import {
  count,
  desc,
  eq,
  getTableColumns,
  max,
  min,
  sql,
  sum,
  WithSubquery,
} from "drizzle-orm";
import { parsePrismaWhere } from "~/util/prismaToDrizzleWhereConditions";
import assert from "assert";
import * as drizzleTables from "~/../drizzle/schema";
import { findRelationsBetweenTables } from "~/util/findRelationsBetweenTables";
import { AnyPgTable } from "drizzle-orm/pg-core";

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

  const tableAliasMapper: Record<string, WithSubquery> = {};
  const tableAliases: WithSubquery[] = [];
  const tablesToAlias =
    jsonColumnSchema
      ?.map((jsonCol) => jsonCol.tableName)
      .filter((t) => t !== table) || [];
  for (const table of tablesToAlias) {
    const jsonSchemaForTable = jsonColumnSchema?.find(
      (jsonCol) => jsonCol.tableName === table
    );
    const jsonCols =
      jsonSchemaForTable?.jsonSchema.map((col) => col.name) || [];
    if (jsonCols.length === 0) {
      continue;
    }
    const jsonColumnName = jsonSchemaForTable?.jsonColumnName;

    const tableAlias = db.$with(`${table}Alias`).as(
      db
        .select({
          ...getTableColumns(tables[table]),
          ...jsonCols.reduce((acc: Record<string, unknown>, col) => {
            acc[col] = sql
              .raw(
                `cast((${jsonColumnName}->>'${col}') as ${
                  jsonSchemaForTable?.jsonSchema.find(
                    (jCol) => jCol.name === col
                  )?.type === "String"
                    ? "Text"
                    : "Numeric"
                })`
              )
              .as(col);
            return acc;
          }, {}),
        })
        .from(tables[table])
    );
    tableAliases.push(tableAlias);
    tableAliasMapper[table] = tableAlias;
  }

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

  const joinEntry = Object.entries(
    operationParameters.groupBy[0]?.join ?? {}
  )[0];
  const [joinTable, joinColumn] = joinEntry
    ? joinEntry
    : [undefined, undefined];

  const joinTableAlias: AnyPgTable | WithSubquery | undefined = joinTable
    ? tableAliasMapper[joinTable] ?? tables[joinTable]
    : undefined;

  const dbQuery = db
    .with(tableAlias)
    .select({
      [operationParameters.groupBy[0].column]:
        tableAlias[operationParameters.groupBy[0].column],
      ...selectFields,
      // @ts-expect-error - We dont know the columns of joinTableAlias
      ...(joinTable ? { [joinColumn]: joinTableAlias[joinColumn] } : {}),
    })
    .from(tableAlias)
    .groupBy(
      tableAlias[operationParameters.groupBy[0].column],
      // @ts-expect-error - We dont know the columns of joinTableAlias
      ...(joinTable ? [joinTableAlias[joinColumn]] : [])
    )
    .orderBy(desc(count(tableAlias["id"])));

  if (joinTable) {
    const [currentTableKey, relatedTableKey] = findRelationsBetweenTables(
      tables[table],
      tables[joinTable]
    );
    dbQuery.leftJoin(
      tables[joinTable],
      // @ts-expect-error - We dont know the columns of joinTableAlias
      eq(tableAlias[currentTableKey], joinTableAlias[relatedTableKey])
    );
  }

  return dbQuery;
}
