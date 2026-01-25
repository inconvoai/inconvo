// @ts-nocheck
import { sql, type Kysely } from "kysely";
import { loadTestEnv, getTestContext } from "../loadTestEnv";

describe("MSSQL aggregate Operation", () => {
  let db: Kysely<any>;
  let QuerySchema: (typeof import("~/types/querySchema"))["QuerySchema"];
  let aggregate: (typeof import("~/operations/aggregate"))["aggregate"];
  let ctx: Awaited<ReturnType<typeof getTestContext>>;

  beforeAll(async () => {
    loadTestEnv("mssql");

    jest.resetModules();
    delete (globalThis as any).__INCONVO_KYSELY_DB__;

    QuerySchema = (await import("~/types/querySchema")).QuerySchema;
    aggregate = (await import("~/operations/aggregate")).aggregate;
    const { getDb } = await import("~/dbConnection");
    db = await getDb();
    ctx = await getTestContext();
  });

  afterAll(async () => {
    if (db) await db.destroy();
  });

  test("What is the highest subtotal we have recorded on an order?", async () => {
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

    const expectedRows = await db
      .selectFrom("orders")
      .select([
        sql<number>`AVG(CAST(subtotal AS FLOAT))`.as("avg_subtotal"),
        sql<number>`SUM(CAST(subtotal AS FLOAT))`.as("sum_subtotal"),
        sql<number>`MIN(subtotal)`.as("min_subtotal"),
        sql<number>`MAX(subtotal)`.as("max_subtotal"),
        sql<number>`COUNT(subtotal)`.as("count_subtotal"),
      ])
      .execute();

    const expected = expectedRows[0] ?? {};
    const aggregateResult =
      "data" in response ? (response.data as any) : (response as any);

    expect(aggregateResult._avg["orders.subtotal"]).toBeCloseTo(
      Number(expected.avg_subtotal),
      9,
    );
    expect(aggregateResult._sum["orders.subtotal"]).toBeCloseTo(
      Number(expected.sum_subtotal),
      4,
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

  test("What is the highest subtotal for an order priced over $100?", async () => {
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
        median: null,
      },
    };

    const parsed = QuerySchema.parse(iql);
    const response = await aggregate(db, parsed, ctx);

    const expectedRows = await db
      .selectFrom("orders")
      .where("subtotal", ">", 100)
      .select([
        sql<number>`AVG(CAST(subtotal AS FLOAT))`.as("avg_subtotal"),
        sql<number>`SUM(CAST(subtotal AS FLOAT))`.as("sum_subtotal"),
        sql<number>`MIN(subtotal)`.as("min_subtotal"),
        sql<number>`MAX(subtotal)`.as("max_subtotal"),
        sql<number>`COUNT(subtotal)`.as("count_subtotal"),
      ])
      .execute();

    const expected = expectedRows[0] ?? {};
    const aggregateResult =
      "data" in response ? (response.data as any) : (response as any);

    expect(aggregateResult._avg["orders.subtotal"]).toBeCloseTo(
      Number(expected.avg_subtotal),
      9,
    );
    expect(aggregateResult._sum["orders.subtotal"]).toBeCloseTo(
      Number(expected.sum_subtotal),
      4,
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

  test("deduplicates overlapping join hops to prevent MS SQL error 1013", async () => {
    // This test verifies that when multiple joins share the same hop,
    // the query doesn't fail with "same exposed names" error (MS SQL 1013)
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
    // This would throw MS SQL error 1013 before the fix
    const response = await aggregate(db, parsed, ctx);
    const aggregateResult =
      "data" in response ? (response.data as any) : (response as any);

    // Verify the query executed successfully and returned valid results
    const expectedRows = await db
      .selectFrom("orders as o")
      .innerJoin("products as p", "p.id", "o.product_id")
      .select(sql<number>`SUM(o.subtotal)`.as("sum_subtotal"))
      .execute();

    const expected = expectedRows[0] ?? { sum_subtotal: 0 };
    expect(aggregateResult._sum["orders.subtotal"]).toBeCloseTo(
      Number(expected.sum_subtotal),
      4,
    );
  });
});
