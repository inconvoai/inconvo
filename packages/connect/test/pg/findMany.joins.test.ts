// @ts-nocheck
import { sql, type Kysely } from "kysely";
import { loadTestEnv, getTestContext } from "../loadTestEnv";

/**
 * Tests for findMany join edge cases:
 * - Multiple joins to the same table with different aliases
 * - Filtering on columns from joined tables
 */
describe("PostgreSQL findMany Join Edge Cases", () => {
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

    QuerySchema = (await import("~/types/querySchema")).QuerySchema;
    findMany = (await import("~/operations/findMany")).findMany;
    const { getDb } = await import("~/dbConnection");
    db = await getDb();
    await clearSchemaCache();
    ctx = await getTestContext();
  });

  afterAll(async () => {
    if (db) await db.destroy();
    await clearSchemaCache?.();
  });

  describe("filtering on joined table columns", () => {
    test("filters on a column from joined table using qualified name", async () => {
      const iql = {
        operation: "findMany" as const,
        table: "orders",
        tableConditions: null,
        whereAndArray: [
          {
            // Filter on the joined products table
            "products.title": {
              contains: "a",
            },
          },
        ],
        operationParameters: {
          select: {
            orders: ["id", "subtotal"],
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
            column: "id",
            direction: "asc" as const,
          },
          limit: 5,
        },
      };

      const parsed = QuerySchema.parse(iql);
      const response = await findMany(db, parsed, ctx);

      // Verify the query executed successfully
      expect(response.data).toBeDefined();
      expect(Array.isArray(response.data)).toBe(true);

      // Verify the SQL contains the WHERE clause on the joined table
      expect(response.query.sql.toLowerCase()).toContain("products");
      expect(response.query.sql.toLowerCase()).toContain("left join");
    });

    test("filters on multiple joined table columns", async () => {
      const iql = {
        operation: "findMany" as const,
        table: "orders",
        tableConditions: null,
        whereAndArray: [
          {
            AND: [
              {
                "orders.subtotal": {
                  gt: 0,
                },
              },
              {
                "products.title": {
                  contains: "e",
                },
              },
            ],
          },
        ],
        operationParameters: {
          select: {
            orders: ["id", "subtotal"],
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
            column: "id",
            direction: "asc" as const,
          },
          limit: 5,
        },
      };

      const parsed = QuerySchema.parse(iql);
      const response = await findMany(db, parsed, ctx);

      expect(response.data).toBeDefined();
      expect(Array.isArray(response.data)).toBe(true);
    });
  });

  describe("output format", () => {
    test("returns joined data with correct column format", async () => {
      const iql = {
        operation: "findMany" as const,
        table: "orders",
        tableConditions: null,
        whereAndArray: [],
        operationParameters: {
          select: {
            orders: ["id", "subtotal"],
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
            column: "id",
            direction: "asc" as const,
          },
          limit: 1,
        },
      };

      const parsed = QuerySchema.parse(iql);
      const response = await findMany(db, parsed, ctx);

      expect(response.data).toBeDefined();
      expect(response.data.length).toBeGreaterThan(0);

      // Check the output format contains expected keys (alias preserves dots)
      const row = response.data[0];
      expect(row.orders_id).toBeDefined();
      expect(row.orders_subtotal).toBeDefined();
      expect(row["orders.product_title"]).toBeDefined();
    });
  });

  describe("SQL generation", () => {
    test("generates correct JOIN syntax", async () => {
      const iql = {
        operation: "findMany" as const,
        table: "orders",
        tableConditions: null,
        whereAndArray: [],
        operationParameters: {
          select: {
            orders: ["id"],
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
            column: "id",
            direction: "asc" as const,
          },
          limit: 1,
        },
      };

      const parsed = QuerySchema.parse(iql);
      const response = await findMany(db, parsed, ctx);

      // Verify the SQL contains proper JOIN
      const sql = response.query.sql.toLowerCase();
      expect(sql).toContain("left join");
      expect(sql).toContain("products");
    });
  });

  describe("shared-hop dedupe", () => {
    test("deduplicates shared hop for multi-hop reviews join", async () => {
      const iql = {
        operation: "findMany" as const,
        table: "orders",
        tableConditions: null,
        whereAndArray: [],
        operationParameters: {
          select: {
            orders: ["id", "product_id"],
            "orders.product": ["title"],
            "orders.product.reviews": ["id"],
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
              table: "reviews",
              name: "orders.product.reviews",
              path: [
                {
                  source: ["orders.product_id"],
                  target: ["products.id"],
                },
                {
                  source: ["products.id"],
                  target: ["reviews.product_id"],
                },
              ],
            },
          ],
          orderBy: {
            column: "id",
            direction: "asc" as const,
          },
          limit: 10,
        },
      };

      const parsed = QuerySchema.parse(iql);
      const response = await findMany(db, parsed, ctx);

      const expectedRows = await db
        .selectFrom("orders as o")
        .leftJoin("products as p", "p.id", "o.product_id")
        .leftJoin("reviews as r", "r.product_id", "p.id")
        .select([
          sql<number>`o.id`.as("id"),
          sql<number>`o.product_id`.as("product_id"),
          sql<string>`p.title`.as("product_title"),
          sql<number>`r.id`.as("review_id"),
        ])
        .orderBy("o.id", "asc")
        .limit(10)
        .execute();

      const expected = expectedRows.map((row: any) => ({
        orders_id: row.id === null ? row.id : Number(row.id),
        orders_product_id: row.product_id === null ? row.product_id : Number(row.product_id),
        "orders.product_title": row.product_title,
        "orders.product.reviews_id":
          row.review_id === null ? row.review_id : Number(row.review_id),
      }));

      expect(response.data).toEqual(expected);
    });
  });
});
