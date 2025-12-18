// @ts-nocheck
import { sql, type Kysely } from "kysely";
import { loadTestEnv } from "../loadTestEnv";

describe("MSSQL count Operation", () => {
  let db: Kysely<any>;
  let QuerySchema: typeof import("~/types/querySchema")["QuerySchema"];
  let count: typeof import("~/operations/count")["count"];

  beforeAll(async () => {
    loadTestEnv("mssql");

    jest.resetModules();
    delete (globalThis as any).__INCONVO_KYSELY_DB__;

    QuerySchema = (await import("~/types/querySchema")).QuerySchema;
    count = (await import("~/operations/count")).count;
    const { getDb } = await import("~/dbConnection");
    db = await getDb();
  });

  afterAll(async () => {
    if (db) await db.destroy();
  });

  test("How many orders have we recorded?", async () => {
    const iql = {
      table: "orders",
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
      .select(sql<number>`COUNT(*)`.as("order_count"))
      .execute();

    const expected = expectedRows[0] ?? { order_count: 0 };
    const result = response.data ?? response;

    expect(result).toEqual({
      _count: {
        "orders.id": Number(expected.order_count),
      },
    });
  });

  test("How many orders do we have with a subtotal over $500?", async () => {
    const iql = {
      table: "orders",
      whereAndArray: [
        {
          subtotal: {
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
      .select(sql<number>`COUNT(*)`.as("order_count"))
      .execute();

    const expected = expectedRows[0] ?? { order_count: 0 };
    const result = response.data ?? response;

    expect(result).toEqual({
      _count: {
        "orders.id": Number(expected.order_count),
      },
    });
  });

  test("How many orders reference a product and how many unique products appear?", async () => {
    const iql = {
      table: "orders",
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
        sql<number>`CAST(COUNT(p.id) AS INT)`.as("total_product_rows"),
        sql<number>`CAST(COUNT(DISTINCT p.title) AS INT)`.as(
          "distinct_product_titles"
        ),
      ])
      .execute();

    const expected =
      expectedRows[0] ?? {
        total_product_rows: 0,
        distinct_product_titles: 0,
      };
    const result = response.data ?? response;

    expect(result).toEqual({
      _count: {
        "orders.products.id": Number(expected.total_product_rows),
      },
      _countDistinct: {
        "orders.products.title": Number(expected.distinct_product_titles),
      },
    });
  });

  test("How many unique customers placed an order?", async () => {
    const iql = {
      table: "orders",
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
      .select(sql<number>`CAST(COUNT(DISTINCT id) AS INT)`.as("distinct_order_ids"))
      .execute();

    const expected = expectedRows[0] ?? { distinct_order_ids: 0 };
    const result = response.data ?? response;

    expect(result).toEqual({
      _count: {},
      _countDistinct: {
        "orders.id": Number(expected.distinct_order_ids),
      },
    });
  });

  test("Rejects count operations without metrics", () => {
    expect(() =>
      QuerySchema.parse({
        table: "orders",
        whereAndArray: [],
        operation: "count" as const,
        operationParameters: {
          count: null,
          countDistinct: null,
        },
      })
    ).toThrow(/at least one metric/);
  });
});
