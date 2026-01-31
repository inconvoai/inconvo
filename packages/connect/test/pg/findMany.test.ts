// @ts-nocheck
import { sql } from "kysely";
import type { Kysely } from "kysely";
import { loadTestEnv, getTestContext } from "../loadTestEnv";

const normalizeRows = (rows: any[]) =>
  rows.map((row) => ({
    ...row,
    orders_created_at:
      row.orders_created_at && typeof row.orders_created_at.toISOString === "function"
        ? row.orders_created_at.toISOString()
        : row.orders_created_at,
  }));

describe("PostgreSQL findMany Operation", () => {
  let db: Kysely<any>;
  let QuerySchema: (typeof import("~/types/querySchema"))["QuerySchema"];
  let findMany: (typeof import("~/operations/findMany"))["findMany"];
  let clearSchemaCache: (typeof import("~/util/schemaCache"))["clearSchemaCache"];
  let ctx: Awaited<ReturnType<typeof getTestContext>>;

  beforeAll(async () => {
    loadTestEnv("postgresql");

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

    const expectedRows = await db
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
      .limit(1)
      .execute();

    // New flat output format: {table}_{column} for base, {alias_with_dots_as_underscores}_{column} for joins
    const expected = expectedRows.map((row: any) => ({
      orders_id: Number(row.id),
      orders_revenue: Number(row.revenue),
      orders_created_at: row.created_at,
      "orders.product_title": row.product_title,
    }));

    expect(normalizeRows(response.data)).toEqual(normalizeRows(expected));
  }, 15000);

  test("uses stored computed columns when not provided in query payload", async () => {
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

    const expectedRows = await db
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
      .limit(1)
      .execute();

    // New flat output format: {table}_{column} for base, {alias_with_dots_as_underscores}_{column} for joins
    const expected = expectedRows.map((row: any) => ({
      orders_id: Number(row.id),
      orders_revenue: Number(row.revenue),
      orders_created_at: row.created_at,
      "orders.product_title": row.product_title,
    }));

    expect(normalizeRows(response.data)).toEqual(normalizeRows(expected));
  }, 15000);

  test("deduplicates overlapping join hops", async () => {
    // This test verifies that when multiple joins share the same hop,
    // the query doesn't fail with duplicate join errors
    const iql = {
      operation: "findMany" as const,
      table: "orders",
      tableConditions: null,
      whereAndArray: [],
      operationParameters: {
        select: {
          orders: ["id", "subtotal"],
          productJoin1: ["title"],
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
            // Second join with the exact same hop - should be deduplicated
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
      .innerJoin("products as p", "p.id", "o.product_id")
      .select([
        sql<number>`o.id`.as("id"),
        sql<number>`o.subtotal`.as("subtotal"),
        sql<string>`p.title`.as("title"),
      ])
      .orderBy("o.id", "asc")
      .limit(5)
      .execute();

    expect(response.data.length).toBe(expectedRows.length);
    // Verify we got data back - the exact format may vary but we should have rows
    if (expectedRows.length > 0) {
      expect(response.data[0]).toHaveProperty("orders_id");
      expect(response.data[0]).toHaveProperty("orders_subtotal");
      expect(response.data[0]).toHaveProperty("productJoin1_title");
    }
  }, 15000);
});
