// @ts-nocheck
import { sql, type Kysely } from "kysely";
import { loadTestEnv, getTestContext } from "../loadTestEnv";
import {
  setupMultiHopFixture,
  teardownMultiHopFixture,
  type MultiHopFixture,
} from "../utils/multiHopFixture";

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
  let clearAugmentedSchemaCache:
    | (typeof import("~/util/augmentedSchemaCache"))["clearAugmentedSchemaCache"]
    | undefined;
  let ctx: Awaited<ReturnType<typeof getTestContext>>;
  let multiHopFixture: MultiHopFixture | null = null;

  beforeAll(async () => {
    loadTestEnv("postgresql");

    jest.resetModules();
    delete (globalThis as any).__INCONVO_KYSELY_DB__;

    ({ clearSchemaCache } = await import("~/util/schemaCache"));
    ({ clearAugmentedSchemaCache } = await import("~/util/augmentedSchemaCache"));

    QuerySchema = (await import("~/types/querySchema")).QuerySchema;
    findMany = (await import("~/operations/findMany")).findMany;
    const { getDb } = await import("~/dbConnection");
    db = await getDb();
    multiHopFixture = await setupMultiHopFixture(db);

    await clearSchemaCache();
    clearAugmentedSchemaCache?.();
    ctx = await getTestContext();
  });

  afterAll(async () => {
    if (db && multiHopFixture) {
      await teardownMultiHopFixture(db, multiHopFixture);
    }
    if (db) await db.destroy();
    await clearSchemaCache?.();
    clearAugmentedSchemaCache?.();
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

  describe("multi-hop aliasing", () => {
    test("deduplicates shared first hop in multi-hop joins", async () => {
      if (!multiHopFixture) {
        throw new Error("Missing multi-hop fixture setup.");
      }

      const aliasPrimary = "primaryMid";
      const aliasSecondary = "secondaryMid";
      const aliasEnd = `${aliasSecondary}.end`;

      const baseIdKey = `${multiHopFixture.baseTable}_${multiHopFixture.baseColumns.id}`;
      const basePrimaryKey = `${multiHopFixture.baseTable}_${multiHopFixture.baseColumns.primaryMidId}`;
      const baseSecondaryKey = `${multiHopFixture.baseTable}_${multiHopFixture.baseColumns.secondaryMidId}`;
      const primaryMidKey = `${aliasPrimary}_${multiHopFixture.midColumns.id}`;
      const secondaryMidKey = `${aliasSecondary}_${multiHopFixture.midColumns.id}`;
      const endLabelKey = `${aliasEnd}_${multiHopFixture.endColumns.label}`;

      const iql = {
        operation: "findMany" as const,
        table: multiHopFixture.baseTable,
        tableConditions: null,
        whereAndArray: [],
        operationParameters: {
          select: {
            [multiHopFixture.baseTable]: [
              multiHopFixture.baseColumns.id,
              multiHopFixture.baseColumns.primaryMidId,
              multiHopFixture.baseColumns.secondaryMidId,
            ],
            [aliasPrimary]: [multiHopFixture.midColumns.id],
            [aliasSecondary]: [multiHopFixture.midColumns.id],
            [aliasEnd]: [multiHopFixture.endColumns.label],
          },
          joins: [
            {
              table: multiHopFixture.midTable,
              name: aliasPrimary,
              path: [
                {
                  source: [
                    `${multiHopFixture.baseTable}.${multiHopFixture.baseColumns.primaryMidId}`,
                  ],
                  target: [
                    `${multiHopFixture.midTable}.${multiHopFixture.midColumns.id}`,
                  ],
                },
              ],
            },
            {
              table: multiHopFixture.midTable,
              name: aliasSecondary,
              path: [
                {
                  source: [
                    `${multiHopFixture.baseTable}.${multiHopFixture.baseColumns.secondaryMidId}`,
                  ],
                  target: [
                    `${multiHopFixture.midTable}.${multiHopFixture.midColumns.id}`,
                  ],
                },
              ],
            },
            {
              table: multiHopFixture.endTable,
              name: aliasEnd,
              path: [
                {
                  source: [
                    `${multiHopFixture.baseTable}.${multiHopFixture.baseColumns.secondaryMidId}`,
                  ],
                  target: [
                    `${multiHopFixture.midTable}.${multiHopFixture.midColumns.id}`,
                  ],
                },
                {
                  source: [
                    `${multiHopFixture.midTable}.${multiHopFixture.midColumns.endId}`,
                  ],
                  target: [
                    `${multiHopFixture.endTable}.${multiHopFixture.endColumns.id}`,
                  ],
                },
              ],
            },
          ],
          orderBy: {
            column: multiHopFixture.baseColumns.id,
            direction: "asc" as const,
          },
          limit: 10,
        },
      };

      const parsed = QuerySchema.parse(iql);
      const response = await findMany(db, parsed, ctx);

      const expectedRows = await db
        .selectFrom(`${multiHopFixture.baseTable} as base`)
        .leftJoin(
          `${multiHopFixture.midTable} as primary_mid`,
          `primary_mid.${multiHopFixture.midColumns.id}`,
          `base.${multiHopFixture.baseColumns.primaryMidId}`,
        )
        .leftJoin(
          `${multiHopFixture.midTable} as secondary_mid`,
          `secondary_mid.${multiHopFixture.midColumns.id}`,
          `base.${multiHopFixture.baseColumns.secondaryMidId}`,
        )
        .leftJoin(
          `${multiHopFixture.endTable} as end_tbl`,
          `end_tbl.${multiHopFixture.endColumns.id}`,
          `secondary_mid.${multiHopFixture.midColumns.endId}`,
        )
        .select([
          sql`${sql.ref("base")}.${sql.ref(multiHopFixture.baseColumns.id)}`.as(
            baseIdKey,
          ),
          sql`${sql.ref("base")}.${sql.ref(multiHopFixture.baseColumns.primaryMidId)}`.as(
            basePrimaryKey,
          ),
          sql`${sql.ref("base")}.${sql.ref(multiHopFixture.baseColumns.secondaryMidId)}`.as(
            baseSecondaryKey,
          ),
          sql`${sql.ref("primary_mid")}.${sql.ref(multiHopFixture.midColumns.id)}`.as(
            primaryMidKey,
          ),
          sql`${sql.ref("secondary_mid")}.${sql.ref(multiHopFixture.midColumns.id)}`.as(
            secondaryMidKey,
          ),
          sql`${sql.ref("end_tbl")}.${sql.ref(multiHopFixture.endColumns.label)}`.as(
            endLabelKey,
          ),
        ])
        .orderBy(`base.${multiHopFixture.baseColumns.id}`, "asc")
        .limit(10)
        .execute();

      expect(response.data).toEqual(expectedRows);
    });
  });
});
