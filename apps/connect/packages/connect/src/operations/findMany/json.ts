import { PrismaClient } from "@prisma/client";
import assert from "assert";
// import { drizzle } from "drizzle-orm/prisma/pg";
import { drizzle } from "drizzle-orm/node-postgres";
import { type Query } from "~/types/querySchema";
import { parsePrismaWhere } from "~/util/prismaToDrizzleWhereConditions";
import * as drizzleTables from "../../../drizzle/schema";
import { eq, sql, WithSubquery } from "drizzle-orm";
import { findRelationsBetweenTables } from "~/util/findRelationsBetweenTables";

const tables: Record<string, any> = drizzleTables;

export async function findManyJson(prisma: PrismaClient, query: Query) {
  assert(query.operation === "findMany", "Invalid inconvo operation");
  const { table, operation, whereAndArray, operationParameters } = query;

  const db = drizzle("postgresql://root:notroot@localhost:1122/example-db");
  const drizzleWhere = parsePrismaWhere(tables[table], whereAndArray);

  const { columns, orderBy, limit } = operationParameters;

  const selectColsPerTable: Record<string, string[] | null> = {};
  Object.entries(query.operationParameters.columns).forEach(
    ([tableRelations, value]) => {
      const colName = tableRelations.split(".").at(-1);
      selectColsPerTable[colName] = value;
    }
  );

  const needCtes =
    Object.keys(query.operationParameters.columns).filter(
      (table) => table !== query.table
    ).length > 0;

  const tableLevels = ["bid", "event", "lot"].reverse();

  const tablesAlias = [];

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

  tablesAlias.push(lotAlias);
  const tableAliasMapper: {
    [key: string]: WithSubquery;
  } = {
    lot: lotAlias,
  };

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

      if (index === 0) {
        const jsonFields = selectColsPerTable[table].map(
          (col) => sql`${col}::text, ${tableSchema[col]}`
        );
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
                json_data:
                  sql`COALESCE(json_agg( ${cte["json_data"]}), '[]')`.as(
                    "json_data"
                  ),
              })
              .from(cte)
              .groupBy(cte[currentTableKey])
          );
          ctes.push(groupedCte);
        }
      } else {
        const previousTableLinks = tableLinks[index - 1];
        const previousTable = ctes[ctes.length - 1];
        const previousTableName = tableLevels[index - 1];
        const jsonFields = (
          selectColsPerTable[table]?.map(
            (col) => sql`${col}::text, ${tableSchema[col]}`
          ) || []
        ).concat(
          sql`${previousTableName}::text, ${previousTable["json_data"]}`
        );
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
                json_data:
                  sql`COALESCE(json_agg( ${cte["json_data"]}), '[]')`.as(
                    "json_data"
                  ),
              })
              .from(cte)
              .groupBy(cte[currentTableKey])
          );
          ctes.push(groupedCte);
        }
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
  const response = await db
    .with(...tablesAlias, ...ctes)
    .select({
      ...rootSelect,
      event: ctes[ctes.length - 1]["json_data"],
    })
    .from(tables[query.table])
    .leftJoin(
      ctes[ctes.length - 1],
      eq(tables[query.table][finalLink[1]], ctes[ctes.length - 1][finalLink[0]])
    )
    .where(drizzleWhere);

  return response;
}
