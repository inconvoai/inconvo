// @ts-nocheck
import { sql } from "kysely";
import type { Kysely } from "kysely";
import { loadTestEnv } from "../loadTestEnv";

describe("MySQL findMany Operation", () => {
  let db: Kysely<any>;
  let QuerySchema: (typeof import("~/types/querySchema"))["QuerySchema"];
  let findMany: (typeof import("~/operations/findMany"))["findMany"];

  beforeAll(async () => {
    loadTestEnv("mysql");

    jest.resetModules();
    delete (globalThis as any).__INCONVO_KYSELY_DB__;

    QuerySchema = (await import("~/types/querySchema")).QuerySchema;
    findMany = (await import("~/operations/findMany")).findMany;
    const { getDb } = await import("~/dbConnection");
    db = await getDb();
  });

  afterAll(async () => {
    if (db) await db.destroy();
  });

  test("Which order recorded the highest subtotal and what product was sold?", async () => {
    const iql = {
      operation: "findMany" as const,
      table: "orders",
      tableConditions: null,
      whereAndArray: [
        {
          "orders.subtotal": {
            gt: 0,
          },
        },
      ],
      operationParameters: {
        select: {
          orders: ["id", "subtotal", "created_at"],
          "orders.product": ["title"],
        },
        joins: [
          {
            table: "products",
            name: "orders.product",
            path: [
              {
                source: ["orders.product_id"],
                target: ["products.id"],
              },
            ],
          },
        ],
        orderBy: {
          column: "subtotal",
          direction: "desc" as const,
        },
        limit: 1,
      },
    };

    const parsed = QuerySchema.parse(iql);
    const response = await findMany(db, parsed);

    const expectedRows = await db
      .selectFrom("orders as o")
      .innerJoin("products as p", "p.id", "o.product_id")
      .select([
        sql<number>`o.id`.as("id"),
        sql<number>`o.subtotal`.as("subtotal"),
        sql<string>`p.title`.as("product_title"),
        sql`o.created_at`.as("created_at"),
      ])
      .where("o.subtotal", ">", 0)
      .orderBy("o.subtotal", "desc")
      .limit(1)
      .execute();

    // New flat output format: {table}_{column} for base, {alias_with_dots_as_underscores}_{column} for joins
    const expected = expectedRows.map((row: any) => ({
      orders_id: Number(row.id),
      orders_subtotal: Number(row.subtotal),
      orders_created_at: row.created_at,
      "orders.product_title": row.product_title,
    }));

    const normalizeRows = (rows: any[]) =>
      rows.map((row) => ({
        ...row,
        orders_created_at:
          row.orders_created_at && typeof row.orders_created_at.toISOString === "function"
            ? row.orders_created_at.toISOString()
            : row.orders_created_at,
      }));

    expect(normalizeRows(response.data)).toEqual(normalizeRows(expected));
  }, 15000);
});
