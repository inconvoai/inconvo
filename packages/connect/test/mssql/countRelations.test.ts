// @ts-nocheck
import { sql, type Kysely } from "kysely";
import { loadTestEnv, getTestContext } from "../loadTestEnv";

describe("MSSQL countRelations Operation", () => {
  let db: Kysely<any>;
  let QuerySchema: (typeof import("~/types/querySchema"))["QuerySchema"];
  let countRelations: (typeof import("~/operations/countRelations"))["countRelations"];
  let ctx: Awaited<ReturnType<typeof getTestContext>>;

  beforeAll(async () => {
    loadTestEnv("mssql");

    jest.resetModules();
    delete (globalThis as any).__INCONVO_KYSELY_DB__;

    QuerySchema = (await import("~/types/querySchema")).QuerySchema;
    countRelations = (await import("~/operations/countRelations"))
      .countRelations;
    const { getDb } = await import("~/dbConnection");
    db = await getDb();
    ctx = await getTestContext();
  });

  afterAll(async () => {
    if (db) await db.destroy();
  });

  test("How many orders does each product have (order by most orders)?", async () => {
    const iql = {
      table: "products",
      tableConditions: null,
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
    const response = await countRelations(db, parsed, ctx);

    const expectedRows = await db
      .selectFrom("products as p")
      .innerJoin("orders as o", "o.product_id", "p.id")
      .select([
        sql`p.id`.as("id"),
        sql`p.title`.as("title"),
        sql<number>`COUNT(*)`.as("orders_count"),
      ])
      .groupBy(["p.id", "p.title"])
      .orderBy("orders_count", "desc")
      .orderBy("p.id", "asc")
      .top(1)
      .execute();

    const expected = expectedRows.map((row: any) => ({
      id: row.id,
      title: row.title,
      orders_count: Number(row.orders_count),
    }));

    const resultRows = Array.isArray(response) ? response : response.data;

    expect(resultRows).toEqual(expected);
  }, 10000);

  test("How many distinct orders has each user placed?", async () => {
    const iql = {
      table: "users",
      tableConditions: null,
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
    const response = await countRelations(db, parsed, ctx);

    const expectedRows = await db
      .selectFrom("users as u")
      .leftJoin("orders as o", "o.user_id", "u.id")
      .select([
        sql`u.id`.as("id"),
        sql<number>`COUNT(DISTINCT o.id)`.as("orders_distinctCount"),
      ])
      .groupBy("u.id")
      .orderBy("u.id", "asc")
      .top(5)
      .execute();

    const expected = expectedRows.map((row: any) => ({
      id: row.id,
      orders_distinctCount: Number(row.orders_distinctCount),
    }));

    const resultRows = Array.isArray(response) ? response : response.data;

    expect(resultRows).toEqual(expected);
  }, 10000);
});
