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

  test("deduplicates identical hops and creates only one SQL join", async () => {
    // This test verifies the fix for MS SQL error 1013.
    // When multiple logical joins share the same hop (source → target),
    // only ONE SQL JOIN should be created, not multiple.
    //
    // Real-world example:
    // - Join 1: Invoice → POS (single hop)
    // - Join 2: Invoice → POS → Location (multi-hop, first hop same as Join 1)
    // Should create: JOIN POS, JOIN Location (not: JOIN POS, JOIN POS, JOIN Location)
    const iql = {
      operation: "findMany" as const,
      table: "orders",
      tableConditions: null,
      whereAndArray: [],
      operationParameters: {
        select: {
          orders: ["id", "subtotal"],
          "orders.product": ["title"],
          "orders.productAlt": ["title"],
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
          {
            // Same hop as above, different logical alias
            // Simulates a multi-hop join where the first hop is shared
            table: "products",
            name: "orders.productAlt",
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

    // Verify query executed successfully (no error 1013)
    expect(response.data).toBeDefined();
    expect(Array.isArray(response.data)).toBe(true);

    // KEY ASSERTION: Verify the SQL has only ONE join to products (not two)
    // Before the fix, this would create two identical JOINs causing error 1013
    const sql = response.query.sql.toLowerCase();
    const productJoinCount = (sql.match(/join[^,]*products/g) || []).length;
    expect(productJoinCount).toBe(1); // Should be exactly 1, not 2

    // Verify both logical aliases return the same data (they share the underlying join)
    if (response.data.length > 0) {
      const row = response.data[0];
      expect(row.orders_id).toBeDefined();
      expect(row.orders_subtotal).toBeDefined();
      expect(row["orders.product_title"]).toBeDefined();
      expect(row["orders.productAlt_title"]).toBeDefined();
      // Both should have the same value since they share the same join
      expect(row["orders.product_title"]).toEqual(row["orders.productAlt_title"]);
    }
  }, 15000);
});
