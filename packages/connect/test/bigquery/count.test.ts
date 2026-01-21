// @ts-nocheck
import { sql, type Kysely } from "kysely";
import { loadTestEnv } from "../loadTestEnv";

describe("BigQuery count Operation", () => {
  let db: Kysely<any>;
  let QuerySchema: (typeof import("~/types/querySchema"))["QuerySchema"];
  let count: (typeof import("~/operations/count"))["count"];

  beforeAll(async () => {
    jest.setTimeout(120000);
    loadTestEnv("bigquery");

    jest.resetModules();
    delete (globalThis as any).__INCONVO_KYSELY_DB__;

    QuerySchema = (await import("~/types/querySchema")).QuerySchema;
    count = (await import("~/operations/count")).count;
    const { getDb } = await import("~/dbConnection");
    db = await getDb();
  });

  afterAll(async () => {
    await db?.destroy?.();
  });

  test("How many orders have we recorded?", async () => {
    const iql = {
      table: "orders",
      tableConditions: null,
      whereAndArray: [],
      operation: "count" as const,
      operationParameters: {
        count: ["orders.id"],
        countDistinct: null,
      },
    };

    const parsed = QuerySchema.parse(iql);
    const response = await count(db, parsed);

    const expectedRows = await db
      .selectFrom("orders")
      .select((eb) => eb.fn.countAll().as("order_count"))
      .execute();

    const expectedCount = Number(expectedRows[0]?.order_count ?? 0);
    const result = response.data ?? response;

    expect(result).toEqual({
      _count: {
        "orders.id": expectedCount,
      },
    });
  });

  test("How many orders do we have with a subtotal over $500?", async () => {
    const iql = {
      table: "orders",
      tableConditions: null,
      whereAndArray: [
        {
          "orders.subtotal": {
            gt: 500,
          },
        },
      ],
      operation: "count" as const,
      operationParameters: {
        count: ["orders.id"],
        countDistinct: null,
      },
    };

    const parsed = QuerySchema.parse(iql);
    const response = await count(db, parsed);

    const expectedRows = await db
      .selectFrom("orders")
      .where("subtotal", ">", 500)
      .select((eb) => eb.fn.countAll().as("order_count"))
      .execute();

    const expectedCount = Number(expectedRows[0]?.order_count ?? 0);
    const result = response.data ?? response;

    expect(result).toEqual({
      _count: {
        "orders.id": expectedCount,
      },
    });
  });

  test("How many orders reference a product and how many unique products appear?", async () => {
    const iql = {
      table: "orders",
      tableConditions: null,
      whereAndArray: [],
      operation: "count" as const,
      operationParameters: {
        joins: [
          {
            table: "products",
            name: "orders.products",
            path: [
              {
                source: ["orders.product_id"],
                target: ["products.id"],
              },
            ],
            joinType: "inner",
          },
        ],
        count: ["orders.products.id"],
        countDistinct: ["orders.products.title"],
      },
    };

    const parsed = QuerySchema.parse(iql);
    const response = await count(db, parsed);

    const expectedRows = await db
      .selectFrom("orders as o")
      .innerJoin("products as p", "p.id", "o.product_id")
      .select((eb) => [
        eb.fn.count("p.id").as("total_product_rows"),
        sql<number>`COUNT(DISTINCT p.title)`.as("distinct_product_titles"),
      ])
      .execute();

    const totals = expectedRows[0] ?? {
      total_product_rows: 0,
      distinct_product_titles: 0,
    };

    const result = response.data ?? response;

    expect(result).toEqual({
      _count: {
        "orders.products.id": Number(totals.total_product_rows ?? 0),
      },
      _countDistinct: {
        "orders.products.title": Number(totals.distinct_product_titles ?? 0),
      },
    });
  });

  test("How many unique customers placed an order?", async () => {
    const iql = {
      table: "orders",
      tableConditions: null,
      whereAndArray: [],
      operation: "count" as const,
      operationParameters: {
        count: null,
        countDistinct: ["orders.id"],
      },
    };

    const parsed = QuerySchema.parse(iql);
    const response = await count(db, parsed);

    const expectedRows = await db
      .selectFrom("orders")
      .select(sql<number>`COUNT(DISTINCT id)`.as("distinct_order_ids"))
      .execute();

    const expectedCount = Number(expectedRows[0]?.distinct_order_ids ?? 0);
    const result = response.data ?? response;

    expect(result).toEqual({
      _count: {},
      _countDistinct: {
        "orders.id": expectedCount,
      },
    });
  });

  test("Rejects count operations without metrics", () => {
    expect(() =>
      QuerySchema.parse({
        table: "orders",
        tableConditions: null,
        whereAndArray: [],
        operation: "count" as const,
        operationParameters: {
          count: null,
          countDistinct: null,
        },
      }),
    ).toThrow(/at least one metric/);
  });

  test("deduplicates overlapping join hops to prevent duplicate table joins", async () => {
    // This test verifies that when multiple joins share the same hop,
    // the query executes successfully without creating duplicate joins
    const iql = {
      table: "orders",
      tableConditions: null,
      whereAndArray: [],
      operation: "count" as const,
      operationParameters: {
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
        count: ["productJoin1.id"],
        countDistinct: null,
      },
    };

    const parsed = QuerySchema.parse(iql);
    const response = await count(db, parsed);
    const result = response.data ?? response;

    // Verify the query executed successfully and returned valid results
    const expectedRows = await db
      .selectFrom("orders as o")
      .innerJoin("products as p", "p.id", "o.product_id")
      .select((eb) => [eb.fn.count("p.id").as("count_product_id")])
      .execute();

    const expected = expectedRows[0] ?? { count_product_id: 0 };
    expect(result._count["productJoin1.id"]).toBe(
      Number(expected.count_product_id),
    );
  });
});
