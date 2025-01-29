import { PrismaClient } from "@prisma/client";
import assert from "assert";
import { drizzle } from "drizzle-orm/prisma/pg";
import { type Query } from "~/types/querySchema";
import { parsePrismaWhere } from "~/util/prismaToDrizzleWhereConditions";
import * as drizzleTables from "../../../drizzle/schema";
import { asc, desc, eq, sql, WithSubquery } from "drizzle-orm";
import { findRelationsBetweenTables } from "~/util/findRelationsBetweenTables";

const tables: Record<string, any> = drizzleTables;

export async function findManyJson(prisma: PrismaClient, query: Query) {
  assert(query.operation === "findMany", "Invalid inconvo operation");
  const { table, operation, whereAndArray, operationParameters } = query;
  const { columns, orderBy, limit } = operationParameters;

  const db = prisma.$extends(drizzle()).$drizzle;
  const drizzleWhere = parsePrismaWhere(tables[table], whereAndArray);

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
    tableSchema: any,
    currentTableKey: string,
    jsonFields: any[],
    groupBy: boolean
  ) {
    const cte = db.$with(`cte${index}${groupBy ? "_" : ""}`).as(
      db
        .select({
          [currentTableKey]: tableSchema[currentTableKey],
          json_data: sql`json_build_object${jsonFields}`.as("json_data"),
        })
        .from(tableSchema)
    );
    ctes.push(cte);

    if (groupBy) {
      const groupedCte = db.$with(`cte${index}`).as(
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
      ctes.push(groupedCte);
    }
  }

  function createSubsequentCte(
    index: number,
    tableSchema: any,
    currentTableKey: string,
    jsonFields: any[],
    previousTable: any,
    previousTableLinks: string[],
    groupBy: boolean
  ) {
    const cte = db.$with(`cte${index}${groupBy ? "_" : ""}`).as(
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
    ctes.push(cte);

    if (groupBy) {
      const groupedCte = db.$with(`cte${index}`).as(
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
      ctes.push(groupedCte);
    }
  }

  // TODO: calculate all needed table aliases dynamically
  const tableAliases: WithSubquery[] = [];
  const lotAlias = db.$with("lotAlias").as(
    db
      .select({
        id: tables["lot"]["id"],
        event_id: tables["lot"]["event_id"],
        route: sql`cast((lotdata->>'route') as Text)`.as("route"),
        volume: sql`cast((lotdata->>'volume') as Text)`.as("volume"),
        service: sql`cast((lotdata->>'service') as Text)`.as("service"),
      })
      .from(tables["lot"])
  );
  tableAliases.push(lotAlias);
  const tableAliasMapper: Record<string, WithSubquery> = {
    lot: lotAlias,
  };

  // TODO: calculate this dynamically
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
  const tableLevels = ["event", "lot", "bid"].reverse();

  const ctes: WithSubquery[] = [];
  const tableLinks: string[][] = [];

  if (needCtes) {
    for (const [index, table] of tableLevels.entries()) {
      if (table === query.table) {
        continue;
      }

      const tableSchema = tableAliasMapper[table] || tables[table];
      const relatedTable = tableLevels[index + 1];
      const [currentTableKey, relatedTableKey, groupBy] =
        findRelationsBetweenTables(tables[table], tables[relatedTable]);
      tableLinks.push([currentTableKey, relatedTableKey]);

      const jsonFields =
        selectColsPerTable[table]?.map(
          //@ts-ignore
          (col) => sql`${col}::text, ${tableSchema[col]}`
        ) || [];

      if (index === 0) {
        createInitialCte(
          index,
          tableSchema,
          currentTableKey,
          jsonFields,
          groupBy
        );
      } else {
        const previousTableLinks = tableLinks[index - 1];
        const previousTable = ctes[ctes.length - 1];
        const previousTableName = tableLevels[index - 1];
        const extendedJsonFields = jsonFields.concat(
          //@ts-ignore
          sql`${previousTableName}::text, ${previousTable["json_data"]}`
        );
        createSubsequentCte(
          index,
          tableSchema,
          currentTableKey,
          extendedJsonFields,
          previousTable,
          previousTableLinks,
          groupBy
        );
      }
    }
  }

  const rootSelect: { [key: string]: any } = (
    query.operationParameters.columns[table] || []
  ).reduce((acc: { [key: string]: any }, column: string) => {
    acc[column] = tables[table][column];
    return acc;
  }, {});

  const finalLink = tableLinks[tableLinks.length - 1];
  const previousTableName = tableLevels[0];
  const dbQuery = db
    .with(...tableAliases, ...ctes)
    .select({
      ...rootSelect,
      // Todo: name this the correct thing
      ...(needCtes
        ? //@ts-ignore
          { event: sql`${ctes[ctes.length - 1]["json_data"]}`.as("event") }
        : {}),
    })
    .from(tables[query.table])

    .where(drizzleWhere);

  if (needCtes) {
    dbQuery.leftJoin(
      ctes[ctes.length - 1],
      //@ts-ignore
      eq(tables[query.table][finalLink[1]], ctes[ctes.length - 1][finalLink[0]])
    );
  }

  if (limit) dbQuery.limit(limit);
  if (orderBy)
    dbQuery.orderBy(
      orderBy.direction === "asc"
        ? asc(tables[table][orderBy.column])
        : desc(tables[table][orderBy.column])
    );

  const response = await dbQuery;
  return response;
}
