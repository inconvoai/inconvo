// @ts-nocheck
import { sql } from "kysely";
import type { Kysely } from "kysely";
import { loadTestEnv, getTestContext } from "../loadTestEnv";

describe("MSSQL findMany Operation", () => {
  let db: Kysely<any>;
  let QuerySchema: (typeof import("~/types/querySchema"))["QuerySchema"];
  let findMany: (typeof import("~/operations/findMany"))["findMany"];
  let clearSchemaCache: (typeof import("~/util/schemaCache"))["clearSchemaCache"];
  let ctx: Awaited<ReturnType<typeof getTestContext>>;

  beforeAll(async () => {
    loadTestEnv("mssql");

    jest.resetModules();
    delete (globalThis as any).__INCONVO_KYSELY_DB__;

    ({ clearSchemaCache } = await import("~/util/schemaCache"));
    await clearSchemaCache();

    QuerySchema = (await import("~/types/querySchema")).QuerySchema;
    findMany = (await import("~/operations/findMany")).findMany;
    const { getDb } = await import("~/dbConnection");
    db = await getDb();
    ctx = await getTestContext();
  });

  afterAll(async () => {
    if (db) await db.destroy();
    await clearSchemaCache?.();
  });

  test("Which order recorded the highest subtotal and what product was sold?", async () => {
    const iql = {
      operation: "findMany" as const,
      table: "orders",
      tableConditions: null,
      whereAndArray: [
        {
          "orders.revenue": {
            gt: 0,
          },
        },
      ],
      operationParameters: {
        select: {
          orders: ["id", "revenue", "created_at"],
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
          column: "revenue",
          direction: "desc" as const,
        },
        limit: 1,
      },
    };

    const parsed = QuerySchema.parse(iql);
    const response = await findMany(db, parsed, ctx);

    const revenueExpr = sql`((o.subtotal - o.discount) + o.tax) * o.quantity`;

    const expectedRow = await db
      .selectFrom("orders as o")
      .innerJoin("products as p", "p.id", "o.product_id")
      .select([
        sql<number>`o.id`.as("id"),
        sql<number>`${revenueExpr}`.as("revenue"),
        sql<string>`p.title`.as("product_title"),
        sql`o.created_at`.as("created_at"),
      ])
      .where(revenueExpr, ">", 0)
      .orderBy(revenueExpr, "desc")
      .executeTakeFirst();

    // New flat output format: {table}_{column} for base, {alias_with_dots_as_underscores}_{column} for joins
    const expected = expectedRow
      ? [
          {
            orders_id: Number(expectedRow.id),
            orders_revenue: Number(expectedRow.revenue),
            orders_created_at: expectedRow.created_at,
            "orders.product_title": expectedRow.product_title,
          },
        ]
      : [];

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

  test("keeps duplicate hops when aliases are referenced", async () => {
    // Multiple joins share the same hop but use different aliases.
    // This must not trigger MS SQL error 1013 and should return both aliases.
    const iql = {
      operation: "findMany" as const,
      table: "orders",
      tableConditions: null,
      whereAndArray: [],
      operationParameters: {
        select: {
          orders: ["id", "subtotal"],
          productJoin1: ["title"],
          productJoin2: ["title"],
        },
        joins: [
          {
            table: "products",
            name: "productJoin1",
            path: [
              {
                source: ["orders.product_id"],
                target: ["products.id"],
              },
            ],
          },
          {
            // Second join with the exact same hop but a different alias
            table: "products",
            name: "productJoin2",
            path: [
              {
                source: ["orders.product_id"],
                target: ["products.id"],
              },
            ],
          },
        ],
        orderBy: {
          column: "id",
          direction: "asc" as const,
        },
        limit: 5,
      },
    };

    const parsed = QuerySchema.parse(iql);
    const response = await findMany(db, parsed, ctx);

    // Verify the query executed successfully and returned valid results
    const expectedRows = await db
      .selectFrom("orders as o")
      .innerJoin("products as p1", "p1.id", "o.product_id")
      .innerJoin("products as p2", "p2.id", "o.product_id")
      .select([
        sql<number>`o.id`.as("id"),
        sql<number>`o.subtotal`.as("subtotal"),
        sql<string>`p1.title`.as("title1"),
        sql<string>`p2.title`.as("title2"),
      ])
      .orderBy("o.id", "asc")
      .top(5)
      .execute();

    expect(response.data.length).toBe(expectedRows.length);
    if (expectedRows.length > 0) {
      const row = response.data[0];
      expect(row).toHaveProperty("orders_id");
      expect(row).toHaveProperty("orders_subtotal");
      expect(row).toHaveProperty("productJoin1_title");
      expect(row).toHaveProperty("productJoin2_title");
      expect(row.productJoin1_title).toEqual(row.productJoin2_title);
    }
  }, 15000);
});
