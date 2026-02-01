// @ts-nocheck
import { sql, type Kysely } from "kysely";
import { loadTestEnv, getTestContext } from "../loadTestEnv";

describe("BigQuery aggregate Operation", () => {
  let db: Kysely<any>;
  let QuerySchema: (typeof import("~/types/querySchema"))["QuerySchema"];
  let aggregate: (typeof import("~/operations/aggregate"))["aggregate"];
  let clearSchemaCache: (typeof import("~/util/schemaCache"))["clearSchemaCache"];
  let clearAugmentedSchemaCache:
    | (typeof import("~/util/augmentedSchemaCache"))["clearAugmentedSchemaCache"]
    | undefined;
  let ctx: Awaited<ReturnType<typeof getTestContext>>;

  beforeAll(async () => {
    jest.setTimeout(120000);
    loadTestEnv("bigquery");

    jest.resetModules();
    delete (globalThis as any).__INCONVO_KYSELY_DB__;

    ({ clearSchemaCache } = await import("~/util/schemaCache"));
    ({ clearAugmentedSchemaCache } = await import("~/util/augmentedSchemaCache"));

    QuerySchema = (await import("~/types/querySchema")).QuerySchema;
    aggregate = (await import("~/operations/aggregate")).aggregate;
    const { getDb } = await import("~/dbConnection");
    db = await getDb();
    await clearSchemaCache();
    clearAugmentedSchemaCache?.();
    ctx = await getTestContext();
  });

  afterAll(async () => {
    await db?.destroy?.();
    await clearSchemaCache?.();
    clearAugmentedSchemaCache?.();
  });

  test("aggregates subtotal stats", async () => {
    const iql = {
      table: "orders",
      tableConditions: null,
      whereAndArray: [],
      operation: "aggregate" as const,
      operationParameters: {
        min: ["orders.subtotal"],
        max: ["orders.subtotal"],
        avg: ["orders.subtotal"],
        sum: ["orders.subtotal"],
        count: ["orders.subtotal"],
        countDistinct: null,
        median: null,
      },
    };

    const parsed = QuerySchema.parse(iql);
    const response = await aggregate(db, parsed, ctx);
    const aggregateResult =
      "data" in response ? (response.data as any) : (response as any);

    const expectedRows = await db
      .selectFrom("orders")
      .select((eb) => [
        eb.fn.avg("subtotal").as("avg_subtotal"),
        eb.fn.sum("subtotal").as("sum_subtotal"),
        eb.fn.min("subtotal").as("min_subtotal"),
        eb.fn.max("subtotal").as("max_subtotal"),
        eb.fn.count("subtotal").as("count_subtotal"),
      ])
      .execute();

    const expected = expectedRows[0] ?? {};

    expect(aggregateResult._avg["orders.subtotal"]).toBeCloseTo(
      Number(expected.avg_subtotal),
      6,
    );
    expect(aggregateResult._sum["orders.subtotal"]).toBeCloseTo(
      Number(expected.sum_subtotal),
      6,
    );
    expect(aggregateResult._min["orders.subtotal"]).toBe(
      Number(expected.min_subtotal),
    );
    expect(aggregateResult._max["orders.subtotal"]).toBe(
      Number(expected.max_subtotal),
    );
    expect(aggregateResult._count["orders.subtotal"]).toBe(
      Number(expected.count_subtotal),
    );
  });

  test("aggregates subtotal stats with filters", async () => {
    const iql = {
      table: "orders",
      tableConditions: null,
      whereAndArray: [
        {
          "orders.subtotal": {
            gt: 100,
          },
        },
      ],
      operation: "aggregate" as const,
      operationParameters: {
        min: ["orders.subtotal"],
        max: ["orders.subtotal"],
        avg: ["orders.subtotal"],
        sum: ["orders.subtotal"],
        count: ["orders.subtotal"],
        countDistinct: null,
        median: ["orders.subtotal"],
      },
    };

    const parsed = QuerySchema.parse(iql);
    const response = await aggregate(db, parsed, ctx);
    const aggregateResult =
      "data" in response ? (response.data as any) : (response as any);

    const expectedRows = await db
      .selectFrom("orders")
      .where("subtotal", ">", 100)
      .select((eb) => [
        eb.fn.avg("subtotal").as("avg_subtotal"),
        eb.fn.sum("subtotal").as("sum_subtotal"),
        eb.fn.min("subtotal").as("min_subtotal"),
        eb.fn.max("subtotal").as("max_subtotal"),
        eb.fn.count("subtotal").as("count_subtotal"),
      ])
      .execute();

    const expected = expectedRows[0] ?? {};

    expect(aggregateResult._avg["orders.subtotal"]).toBeCloseTo(
      Number(expected.avg_subtotal),
      6,
    );
    expect(aggregateResult._sum["orders.subtotal"]).toBeCloseTo(
      Number(expected.sum_subtotal),
      6,
    );
    expect(aggregateResult._min["orders.subtotal"]).toBe(
      Number(expected.min_subtotal),
    );
    expect(aggregateResult._max["orders.subtotal"]).toBe(
      Number(expected.max_subtotal),
    );
    expect(aggregateResult._count["orders.subtotal"]).toBe(
      Number(expected.count_subtotal),
    );
    expect(aggregateResult._median?.["orders.subtotal"]).not.toBeNull();
  });

  test("countDistinct returns unique counts for specified columns", async () => {
    const iql = {
      table: "orders",
      tableConditions: null,
      whereAndArray: [],
      operation: "aggregate" as const,
      operationParameters: {
        min: null,
        max: null,
        avg: null,
        sum: null,
        count: ["orders.id"],
        countDistinct: ["orders.product_id", "orders.organisation_id"],
        median: null,
      },
    };

    const parsed = QuerySchema.parse(iql);
    const response = await aggregate(db, parsed, ctx);
    const aggregateResult =
      "data" in response ? (response.data as any) : (response as any);

    const expectedRows = await db
      .selectFrom("orders")
      .select([
        sql<number>`COUNT(id)`.as("count_id"),
        sql<number>`COUNT(DISTINCT product_id)`.as("distinct_product_id"),
        sql<number>`COUNT(DISTINCT organisation_id)`.as(
          "distinct_organisation_id",
        ),
      ])
      .execute();

    const expected = expectedRows[0] ?? {};

    expect(aggregateResult._count["orders.id"]).toBe(Number(expected.count_id));
    expect(aggregateResult._countDistinct["orders.product_id"]).toBe(
      Number(expected.distinct_product_id),
    );
    expect(aggregateResult._countDistinct["orders.organisation_id"]).toBe(
      Number(expected.distinct_organisation_id),
    );
  });

  test("aggregates across joins with relational filters", async () => {
    const startDate = new Date("2025-10-27T00:00:00.000Z");
    const iql = {
      table: "orders",
      tableConditions: null,
      operation: "aggregate" as const,
      operationParameters: {
        avg: null,
        count: null,
        countDistinct: null,
        joins: [
          {
            joinType: "inner",
            name: "orders.product",
            path: [
              {
                source: ["orders.product_id"],
                target: ["products.id"],
              },
            ],
            table: "products",
          },
        ],
        max: null,
        median: null,
        min: null,
        sum: ["orders.quantity"],
      },
      whereAndArray: [
        {
          "orders.organisation_id": {
            equals: 1,
          },
        },
        {
          AND: [
            {
              "orders.created_at": {
                gte: startDate.toISOString(),
              },
            },
            {
              product: {
                is: {
                  title: {
                    contains_insensitive: "MacBook",
                  },
                },
              },
            },
          ],
        },
      ],
    };

    const parsed = QuerySchema.parse(iql);
    const response = await aggregate(db, parsed, ctx);
    const aggregateResult =
      "data" in response ? (response.data as any) : (response as any);

    const expectedRows = await db
      .selectFrom("orders")
      .innerJoin("products", "orders.product_id", "products.id")
      .where("orders.organisation_id", "=", 1)
      .where("orders.created_at", ">=", startDate)
      .where(({ eb }) =>
        eb(sql`LOWER(products.title)`, "like", sql`LOWER(${`%macbook%`})`),
      )
      .select(({ fn }) => [
        fn.sum<number>("orders.quantity").as("sum_orders_quantity"),
      ])
      .execute();

    const expectedSum = Number(expectedRows[0]?.sum_orders_quantity ?? 0);

    expect(aggregateResult?._sum?.["orders.quantity"]).toBeCloseTo(
      expectedSum,
      6,
    );
  });

  test("deduplicates overlapping join hops to prevent duplicate table joins", async () => {
    // This test verifies that when multiple joins share the same hop,
    // the query executes successfully without creating duplicate joins
    const iql = {
      table: "orders",
      tableConditions: null,
      whereAndArray: [],
      operation: "aggregate" as const,
      operationParameters: {
        min: null,
        max: null,
        avg: null,
        sum: ["orders.subtotal"],
        count: null,
        countDistinct: null,
        median: null,
        joins: [
          {
            table: "products",
            name: "productJoin1",
            joinType: "inner",
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
            joinType: "inner",
            path: [
              {
                source: ["orders.product_id"],
                target: ["products.id"],
              },
            ],
          },
        ],
      },
    };

    const parsed = QuerySchema.parse(iql);
    const response = await aggregate(db, parsed, ctx);
    const aggregateResult =
      "data" in response ? (response.data as any) : (response as any);

    // Verify the query executed successfully and returned valid results
    const expectedRows = await db
      .selectFrom("orders as o")
      .innerJoin("products as p", "p.id", "o.product_id")
      .select((eb) => [eb.fn.sum<number>("o.subtotal").as("sum_subtotal")])
      .execute();

    const expected = expectedRows[0] ?? { sum_subtotal: 0 };
    expect(aggregateResult._sum["orders.subtotal"]).toBeCloseTo(
      Number(expected.sum_subtotal),
      4,
    );
  });
});
