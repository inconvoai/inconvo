import { PrismaClient } from "@prisma/client";
import assert from "assert";
import { drizzle } from "drizzle-orm/prisma/pg";
import { type Query } from "~/types/querySchema";
import { parsePrismaWhere } from "~/util/prismaToDrizzleWhereConditions";
import * as drizzleTables from "../../../drizzle/schema";
import { asc, desc, eq, sql, WithSubquery } from "drizzle-orm";
import { findRelationsBetweenTables } from "~/util/findRelationsBetweenTables";
import { AnyPgTable } from "drizzle-orm/pg-core";

const tables: Record<string, any> = drizzleTables;

export async function findManyJson(prisma: PrismaClient, query: Query) {
  assert(query.operation === "findMany", "Invalid inconvo operation");
  const { table, operation, whereAndArray, operationParameters } = query;
  const { columns, orderBy, limit } = operationParameters;

  const db = prisma.$extends(drizzle()).$drizzle;

  const selectColsPerTable: Record<string, string[] | null> = {};
  Object.entries(columns).forEach(([tableRelations, value]) => {
    const colName = tableRelations.split(".").at(-1);
    if (colName === undefined) return;
    selectColsPerTable[colName] = value;
  });

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

  // TODO: calculate all needed table aliases dynamically
  const tableAliases: WithSubquery[] = [];
  const lotAlias = db.$with("lotAlias").as(
    db
      .select({
        id: tables["lot"]["id"],
        event_id: tables["lot"]["event_id"],
        lotdata: tables["lot"]["lotdata"],
        route: sql`cast((lotdata->>'route') as Text)`.as("route"),
        volume: sql`cast((lotdata->>'volume') as Text)`.as("volume"),
        service: sql`cast((lotdata->>'service') as Text)`.as("service"),
      })
      .from(tables["lot"])
  );
  tableAliases.push(lotAlias);

  const bidAlias = db.$with("bidAlias").as(
    db
      .select({
        id: tables["bid"]["id"],
        lot_id: tables["bid"]["lot_id"],
        event_id: tables["bid"]["event_id"],
        biddata: tables["bid"]["biddata"],
        amount: sql`cast((biddata->>'amount') as Text)`.as("amount"),
        currency: sql`cast((biddata->>'currency') as Text)`.as("currency"),
        supplier: sql`cast((biddata->>'supplier') as Text)`.as("supplier"),
      })
      .from(tables["bid"])
  );
  tableAliases.push(bidAlias);

  const tableAliasMapper: Record<string, WithSubquery> = {
    lot: lotAlias,
    bid: bidAlias,
  };

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
