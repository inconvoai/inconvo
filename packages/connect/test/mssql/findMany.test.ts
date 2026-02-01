// @ts-nocheck
import { sql } from "kysely";
import type { Kysely } from "kysely";
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

describe("MSSQL findMany Operation", () => {
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
    loadTestEnv("mssql");

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
      .top(10)
      .execute();

    expect(normalizeDynamicRows(response.data)).toEqual(
      normalizeDynamicRows(expectedRows),
    );
  }, 15000);
});
