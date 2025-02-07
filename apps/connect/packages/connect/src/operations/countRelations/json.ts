import { type PrismaClient } from "@prisma/client";
import { type Query } from "~/types/querySchema";
import assert from "assert";
import { drizzle } from "drizzle-orm/prisma/pg";
import { parsePrismaWhere } from "~/util/prismaToDrizzleWhereConditions";
import * as drizzleTables from "~/../drizzle/schema";
import { eq, getTableColumns, sql } from "drizzle-orm";
import { AnyPgTable } from "drizzle-orm/pg-core";
import { getTableConfig } from "drizzle-orm/pg-core";
import { findRelationsBetweenTables } from "~/util/findRelationsBetweenTables";

const tables: Record<string, any> = drizzleTables;

export async function countRelationsJson(prisma: PrismaClient, query: Query) {
  assert(query.operation === "countRelations", "Invalid inconvo operation");
  const { table, whereAndArray, operationParameters, jsonColumnSchema } = query;

  const db = prisma.$extends(drizzle()).$drizzle;

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
  );

  // FIXME: there is an issue here if you try to filter on the relations
  // i.e https://www.prisma.io/docs/orm/prisma-client/queries/relation-queries#filter-on-absence-of--to-many-records
  const drizzleWhere = parsePrismaWhere(tableAlias, table, whereAndArray);

  function getTablePrimaryKey(table: AnyPgTable) {
    const { columns } = getTableConfig(table);
    for (const column of columns) {
      if (column.primary) {
        return column.name;
      }
    }
    throw new Error("Table does not have a primary key");
  }

  const relationsToCount =
    operationParameters.relations?.map((table) => {
      const primaryKey = getTablePrimaryKey(tables[table]);
      return sql`${table}::text,  COUNT(DISTINCT ${tables[table][primaryKey]})::numeric`;
    }) || [];

  const rootSelect: { [key: string]: any } = (
    query.operationParameters.columns || []
  ).reduce((acc: { [key: string]: any }, column: string) => {
    acc[column] = tableAlias[column];
    return acc;
  }, {});

  const dbQuery = db
    .with(tableAlias)
    .select({
      ...rootSelect,
      _count: sql<number>`JSON_BUILD_OBJECT${relationsToCount}`.as("_count"),
    })
    .from(tableAlias)
    .where(drizzleWhere);

  for (const joinTable of operationParameters.relations) {
    const [currentTableKey, relatedTableKey] = findRelationsBetweenTables(
      tables[table],
      tables[joinTable]
    );
    dbQuery.leftJoin(
      tables[joinTable],
      eq(tables[joinTable][relatedTableKey], tableAlias[currentTableKey])
    );
  }

  const groupByColumns = operationParameters.columns.map(
    (col) => tableAlias[col]
  );

  dbQuery.groupBy(...groupByColumns);
  const response = await dbQuery;

  return response.length > 0 ? response : 0;
}
