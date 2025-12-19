// @ts-nocheck
import { sql, type Kysely } from "kysely";
import { loadTestEnv } from "../loadTestEnv";

describe("BigQuery countRelations Operation", () => {
  let db: Kysely<any>;
  let QuerySchema: (typeof import("~/types/querySchema"))["QuerySchema"];
  let countRelations: (typeof import("~/operations/countRelations"))["countRelations"];

  beforeAll(async () => {
    jest.setTimeout(120000);
    loadTestEnv("bigquery");

    jest.resetModules();
    delete (globalThis as any).__INCONVO_KYSELY_DB__;

    QuerySchema = (await import("~/types/querySchema")).QuerySchema;
    countRelations = (await import("~/operations/countRelations"))
      .countRelations;
    const { getDb } = await import("~/dbConnection");
    db = await getDb();
  });

  afterAll(async () => {
    await db?.destroy?.();
  });

  test("How many orders does each product have (order by most orders)?", async () => {
    const iql = {
      table: "products",
      whereAndArray: [],
      operation: "countRelations" as const,
      operationParameters: {
        columns: ["id", "title"],
        joins: [
          {
            table: "orders",
            name: "orders",
            path: [
              {
                source: ["products.id"],
                target: ["orders.product_id"],
              },
            ],
          },
        ],
        relationsToCount: [
          {
            name: "orders",
            distinct: null,
          },
        ],
        orderBy: {
          name: "orders",
          direction: "desc" as const,
        },
        limit: 1,
      },
    };

    const parsed = QuerySchema.parse(iql);
    const response = await countRelations(db, parsed);

    const resultRows = Array.isArray(response) ? response : response.data;

    const verification = await db
      .selectFrom("orders as o")
      .innerJoin("products as p", "p.id", "o.product_id")
      .groupBy(["p.id", "p.title"])
      .select([
        sql`p.id`.as("id"),
        sql`p.title`.as("title"),
        sql<number>`COUNT(*)`.as("orders_count"),
      ])
      .orderBy("orders_count", "desc")
      .orderBy("p.id", "asc")
      .limit(1)
      .execute();

    const expected = verification.map((row: any) => ({
      id: row.id,
      title: row.title,
      orders_count: Number(row.orders_count),
    }));

    expect(resultRows).toEqual(expected);
  }, 10000);

  test("How many distinct orders has each user placed?", async () => {
    const iql = {
      table: "users",
      whereAndArray: [],
      operation: "countRelations" as const,
      operationParameters: {
        columns: ["id"],
        joins: [
          {
            table: "orders",
            name: "orders",
            path: [
              {
                source: ["users.id"],
                target: ["orders.user_id"],
              },
            ],
            joinType: "left",
          },
        ],
        relationsToCount: [
          {
            name: "orders",
            distinct: "orders.id",
          },
        ],
        orderBy: null,
        limit: 5,
      },
    };

    const parsed = QuerySchema.parse(iql);
    const response = await countRelations(db, parsed);

    const resultRows = Array.isArray(response) ? response : response.data;
    expect(resultRows.length).toBeLessThanOrEqual(5);

    const ids = resultRows.map((row: any) => row.id);
    if (ids.length === 0) {
      return;
    }

    const expectedRows = await db
      .selectFrom("users as u")
      .leftJoin("orders as o", "o.user_id", "u.id")
      .where("u.id", "in", ids)
      .select((eb) => [
        sql`u.id`.as("id"),
        sql<number>`COUNT(DISTINCT o.id)`.as("orders_distinctCount"),
      ])
      .groupBy("u.id")
      .execute();

    const expectedMap = new Map(
      expectedRows.map((row: any) => [
        row.id,
        Number(row.orders_distinctCount ?? 0),
      ]),
    );

    resultRows.forEach((row: any) => {
      expect(Number(row.orders_distinctCount ?? 0)).toBe(
        expectedMap.get(row.id) ?? 0,
      );
    });
  }, 10000);
});
