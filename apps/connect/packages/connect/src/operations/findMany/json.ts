import { PrismaClient } from "@prisma/client";
import assert from "assert";
// import { drizzle } from "drizzle-orm/prisma/pg";
import { drizzle } from "drizzle-orm/node-postgres";
import { type Query } from "~/types/querySchema";
import { parsePrismaWhere } from "~/util/prismaToDrizzleWhereConditions";
import * as drizzleTables from "../../../drizzle/schema";
import { asc, eq, desc, sql, arrayContains, inArray } from "drizzle-orm";
import { buildDrizzleRelationalSelect } from "~/util/buildDrizzleRelationalSelect";
import { buildSchema } from "~/util/buildSchema";

const tables: Record<string, any> = drizzleTables;

export async function findManyJson(prisma: PrismaClient, query: Query) {
  assert(query.operation === "findMany", "Invalid inconvo operation");
  const { table, operation, whereAndArray, operationParameters } = query;

  // const db = drizzle("postgresql://root:notroot@localhost:1122/example-db");
  const drizzleWhere = parsePrismaWhere(tables[table], whereAndArray);

  const { columns, orderBy, limit } = operationParameters;

  // const schema = buildSchema();

  // const longest = ["event", "lot", "bid"];

  // const ctes: any = [];

  // for (const [index, table] of longest.entries()) {
  //   if (index === 0) {
  //     ctes[index] = db.$with(`cte${index}`).as(
  //       db
  //         .select({
  //           id: tables[table]["id"],
  //           lot_id: tables[table]["lot_id"],
  //           amount: sql<number>`cast((bidData->>'amount') as Numeric)`.as(
  //             "amount"
  //           ),
  //         })
  //         .from(tables[table])
  //     );
  //   } else {
  //     ctes[index] = db.$with(`cte${index}`).as(
  //       db
  //         .select({
  //           lot_id: ctes[index - 1]["lot_id"],
  //           json_data: sql`json_build_object( 'id', ${
  //             ctes[index - 1]["id"]
  //           },'amount', ${ctes[index - 1]["amount"]})`.as("json_data"),
  //         })
  //         .from(ctes[index - 1])
  //     );
  //   }
  // }

  // const ctet2 = db.$with("ctet2").as(
  //   db
  //     .select({
  //       lot_id: ctet1.lot_id,
  //       json_data:
  //         sql`json_build_object( 'id', ${ctet1["id"]},'amount', ${ctet1["amount"]})`.as(
  //           "json_data"
  //         ),
  //     })
  //     .from(ctet1)
  // );

  // const ctet3 = db.$with("ctet3").as(
  //   db
  //     .select({
  //       id: tables["lot"]["id"],
  //       event_id: tables["lot"]["event_id"],
  //       json_data:
  //         sql`(json_build_object( 'id', ${tables["lot"]["id"]}, 'bid', ${ctet2["json_data"]}))`.as(
  //           "json_data"
  //         ),
  //     })
  //     .from(tables["lot"])
  //     .leftJoin(ctet2, eq(tables["lot"]["id"], ctet2["lot_id"]))
  // );

  // const ctet4 = db.$with("ctet4").as(
  //   db
  //     .select({
  //       id: tables["event"]["id"],
  //       json_data: sql`COALESCE(json_agg( ${ctet3["json_data"]}))`.as(
  //         "json_data"
  //       ),
  //     })
  //     .from(tables["event"])
  //     .leftJoin(ctet3, eq(tables["event"]["id"], ctet3["event_id"]))
  //     .groupBy(tables["event"]["id"])
  // );

  // const response = await db
  //   .with(ctet1, ctet2, ctet3, ctet4)
  //   .select({
  //     id: tables["event"]["id"],
  //     lot: ctet4["json_data"],
  //   })
  //   .from(tables["event"])
  //   .leftJoin(ctet4, eq(tables["event"]["id"], ctet4["id"]));

  const db = drizzle({ schema: tables });

  const sqlS = db.query.rental
    .findMany({
      with: {
        inventory: {
          with: {
            film: true,
          },
        },
      },
    })
    .toSQL();

  console.log(sqlS);

  const response = true;

  return response;
}
