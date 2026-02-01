// @ts-nocheck
import { sql, type Kysely } from "kysely";
import { loadTestEnv, getTestContext } from "../loadTestEnv";
import {
  setupMultiHopFixture,
  teardownMultiHopFixture,
  type MultiHopFixture,
} from "../utils/multiHopFixture";

const normalizeDynamicRows = (rows: any[]) =>
  rows.map((row) => {
    const normalized: Record<string, any> = {};
    for (const [key, value] of Object.entries(row)) {
      normalized[key] = value instanceof Date ? value.toISOString() : value;
    }
    return normalized;
  });

describe("BigQuery findMany Operation", () => {
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
    jest.setTimeout(120000);
    loadTestEnv("bigquery");

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
    await db?.destroy?.();
    await clearSchemaCache?.();
    clearAugmentedSchemaCache?.();
  });

  test("Which order recorded the highest revenue and what product was sold?", async () => {
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

    const normalizeRows = (rows: any[]) =>
      rows.map((row) => ({
        ...row,
        orders_created_at:
          row.orders_created_at && typeof row.orders_created_at.toISOString === "function"
            ? row.orders_created_at.toISOString()
            : row.orders_created_at,
      }));

    expect(normalizeRows(response.data)).toEqual(normalizeRows(expected));
  }, 150000);

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
  }, 150000);

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

    expect(normalizeDynamicRows(response.data)).toEqual(
      normalizeDynamicRows(expectedRows),
    );
  }, 150000);
});
