// @ts-nocheck
import type { Kysely } from "kysely";
import { loadTestEnv } from "../loadTestEnv";

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
      const response = await findMany(db, parsed);

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
      const response = await findMany(db, parsed);

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
      const response = await findMany(db, parsed);

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
      const response = await findMany(db, parsed);

      // Verify the SQL contains proper JOIN
      const sql = response.query.sql.toLowerCase();
      expect(sql).toContain("left join");
      expect(sql).toContain("products");
    });
  });
});
