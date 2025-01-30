import { PrismaClient } from "@prisma/client";
import assert from "assert";
import { drizzle } from "drizzle-orm/prisma/pg";
import { type Query } from "~/types/querySchema";
import { parsePrismaWhere } from "~/util/prismaToDrizzleWhereConditions";
import * as drizzleTables from "../../../drizzle/schema";
import { asc, desc, eq, getTableColumns, sql, WithSubquery } from "drizzle-orm";
import { findRelationsBetweenTables } from "~/util/findRelationsBetweenTables";
import { AnyPgTable } from "drizzle-orm/pg-core";

const tables: Record<string, any> = drizzleTables;

export async function findManyJson(prisma: PrismaClient, query: Query) {
  assert(query.operation === "findMany", "Invalid inconvo operation");
  const {
    table,
    operation,
    whereAndArray,
    operationParameters,
    jsonColumnSchema,
  } = query;
  const { columns, orderBy, limit } = operationParameters;

  const db = prisma.$extends(drizzle()).$drizzle;

  const selectColsPerTable: Record<string, string[] | null> = {};
  Object.entries(columns).forEach(([tableRelations, value]) => {
    const colName = tableRelations.split(".").at(-1);
    if (colName === undefined) return;
    selectColsPerTable[colName] = value;
  });

  const tableAliasMapper: Record<string, WithSubquery> = {};
  const tableAliases: WithSubquery[] = [];
  for (const [table, cols] of Object.entries(columns)) {
    if (!cols) continue;
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
                  )
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

  const needCtes =
    Object.keys(columns).filter((table) => table !== query.table).length > 0;

  function createInitialCte(
    index: number,
    table: string,
    tableSchema: any,
    currentTableKey: string,
    jsonFields: any[],
    groupBy: boolean
  ) {
    const cte = db.$with(`cte${table}${index}${groupBy ? "_" : ""}`).as(
      db
        .select({
          [currentTableKey]: tableSchema[currentTableKey],
          json_data: sql`json_build_object${jsonFields}`.as("json_data"),
        })
        .from(tableSchema)
    );

    if (groupBy) {
      const groupedCte = db.$with(`cte${table}${index}`).as(
        db
          .select({
            [currentTableKey]: cte[currentTableKey],
            json_data: sql`COALESCE(json_agg(${cte["json_data"]}), '[]')`.as(
              "json_data"
            ),
          })
          .from(cte)
          .groupBy(cte[currentTableKey])
      );
      return [cte, groupedCte];
    }
    return [cte];
  }

  function createSubsequentCte(
    index: number,
    table: string,
    tableSchema: any,
    currentTableKey: string,
    jsonFields: any[],
    previousTable: any,
    previousTableLinks: string[],
    groupBy: boolean
  ) {
    const cte = db.$with(`cte${table}${index}${groupBy ? "_" : ""}`).as(
      db
        .select({
          [currentTableKey]: tableSchema[currentTableKey],
          json_data: sql`json_build_object${jsonFields}`.as("json_data"),
        })
        .from(tableSchema)
        .leftJoin(
          previousTable,
          eq(
            tableSchema[previousTableLinks[1]],
            previousTable[previousTableLinks[0]]
          )
        )
    );

    if (groupBy) {
      const groupedCte = db.$with(`cte${table}${index}`).as(
        db
          .select({
            [currentTableKey]: cte[currentTableKey],
            json_data: sql`COALESCE(json_agg(${cte["json_data"]}), '[]')`.as(
              "json_data"
            ),
          })
          .from(cte)
          .groupBy(cte[currentTableKey])
      );
      return [cte, groupedCte];
    }
    return [cte];
  }

  const tablePaths = Object.keys(operationParameters.columns)
    .filter((table) => table !== query.table)
    .map((table) => table.split("."));
  const dedupedTablePaths = tablePaths.filter(
    (arr, index, self) =>
      !self.some(
        (other) =>
          other.length > arr.length &&
          other.slice(0, arr.length).every((v, i) => v === arr[i])
      )
  );

  const nestedJsonCtes: WithSubquery[][] = [];
  const outerTableLinks: string[][][] = [];

  if (needCtes) {
    const jsonCtes: WithSubquery[] = [];
    const tableLinks: string[][] = [];
    for (const tablePath of dedupedTablePaths) {
      const reverseTablePath = tablePath.reverse();
      for (const [index, table] of reverseTablePath.entries()) {
        if (table === query.table) {
          continue;
        }

        const tableSchema = tableAliasMapper[table] || tables[table];
        const relatedTable = tablePath[index + 1];
        const [currentTableKey, relatedTableKey, groupBy] =
          findRelationsBetweenTables(tables[table], tables[relatedTable]);
        tableLinks.push([currentTableKey, relatedTableKey]);

        const jsonFields =
          selectColsPerTable[table]?.map(
            //@ts-ignore
            (col) => sql`${col}::text, ${tableSchema[col]}`
          ) || [];

        if (index === 0) {
          const ctes = createInitialCte(
            index,
            table,
            tableSchema,
            currentTableKey,
            jsonFields,
            groupBy
          );
          jsonCtes.push(...ctes);
        } else {
          const previousTableLinks = tableLinks[index - 1];
          const previousTable = jsonCtes[jsonCtes.length - 1];
          const previousTableName = tablePath[index - 1];
          const extendedJsonFields = jsonFields.concat(
            //@ts-ignore
            sql`${previousTableName}::text, ${previousTable["json_data"]}`
          );
          const ctes = createSubsequentCte(
            index,
            table,
            tableSchema,
            currentTableKey,
            extendedJsonFields,
            previousTable,
            previousTableLinks,
            groupBy
          );
          jsonCtes.push(...ctes);
        }
      }
      nestedJsonCtes.push(jsonCtes);
      outerTableLinks.push(tableLinks);
    }
  }

  const tableSchema: AnyPgTable | WithSubquery =
    tableAliasMapper[query.table] || tables[query.table];

  const drizzleWhere = parsePrismaWhere(tableSchema, whereAndArray);

  const rootSelect: { [key: string]: any } = (
    query.operationParameters.columns[table] || []
  ).reduce((acc: { [key: string]: any }, column: string) => {
    //@ts-ignore
    acc[column] = tableSchema[column];
    return acc;
  }, {});

  const dbQuery = db
    .with(...tableAliases, ...nestedJsonCtes.flat())
    .select({
      ...rootSelect,
      ...dedupedTablePaths.reduce(
        (acc: Record<string, any>, tablePath, index) => {
          const tableCte =
            nestedJsonCtes[index][nestedJsonCtes[index].length - 1];
          const tableName = tablePath[1];
          //@ts-ignore
          acc[tableName] = sql`${tableCte["json_data"]}`.as(tableName);
          return acc;
        },
        {}
      ),
    })
    .from(tableSchema)
    .where(drizzleWhere);

  if (needCtes) {
    nestedJsonCtes.forEach((ctes, index) => {
      const tableCte = ctes[ctes.length - 1];
      const finalLink =
        outerTableLinks[index][outerTableLinks[index].length - 1];
      dbQuery.leftJoin(
        tableCte,
        eq(
          // @ts-ignore
          tableSchema[finalLink[1]],
          // @ts-ignore
          tableCte[finalLink[0]]
        )
      );
    });
  }

  if (limit) dbQuery.limit(limit);
  if (orderBy) {
    dbQuery.orderBy(
      orderBy.direction === "asc"
        ? // @ts-ignore
          asc(tableSchema[orderBy.column])
        : // @ts-ignore
          desc(tableSchema[orderBy.column])
    );
  }

  const response = await dbQuery;
  return response;
}
