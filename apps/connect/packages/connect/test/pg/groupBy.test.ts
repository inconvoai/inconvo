// @ts-nocheck
import { sql, type Kysely } from "kysely";
import { loadTestEnv } from "../loadTestEnv";

describe("PostgreSQL groupBy Operation", () => {
  let db: Kysely<any>;
  let QuerySchema: typeof import("~/types/querySchema")["QuerySchema"];
  let groupBy: typeof import("~/operations/groupBy")["groupBy"];

  beforeAll(async () => {
    loadTestEnv("postgresql");

    jest.resetModules();
    delete (globalThis as any).__INCONVO_KYSELY_DB__;

    QuerySchema = (await import("~/types/querySchema")).QuerySchema;
    groupBy = (await import("~/operations/groupBy")).groupBy;
    const { getDb } = await import("~/dbConnection");
    db = await getDb();
  });

  afterAll(async () => {
    if (db) await db.destroy();
  });

  test("Which products drive the highest order subtotal?", async () => {
    const iql = {
      table: "orders",
      whereAndArray: [],
      operation: "groupBy" as const,
      operationParameters: {
        joins: [
          {
            table: "products",
            name: "product",
            path: [
              {
                source: ["orders.product_id"],
                target: ["products.id"],
              },
            ],
            joinType: "inner",
          },
        ],
        groupBy: [
          { type: "column", column: "orders.product_id" },
          { type: "column", column: "products.title" },
        ],
        count: null,
        countDistinct: null,
        sum: ["orders.subtotal"],
        min: null,
        max: null,
        avg: null,
        orderBy: {
          type: "aggregate" as const,
          function: "sum",
          column: "orders.subtotal",
          direction: "desc" as const,
        },
        limit: 10,
      },
    };

    const parsed = QuerySchema.parse(iql);
    const response = await groupBy(db, parsed);

    const expectedRows = await db
      .selectFrom("orders as o")
      .innerJoin("products as p", "p.id", "o.product_id")
      .select([
        sql`o.product_id`.as("product_id"),
        sql`p.title`.as("title"),
        sql<number>`SUM(o.subtotal)::float8`.as("subtotal_sum"),
      ])
      .groupBy(["o.product_id", "p.title"])
      .orderBy("subtotal_sum", "desc")
      .orderBy("product_id", "asc")
      .limit(10)
      .execute();

    const expected = expectedRows.map((row: any) => ({
      "orders.product_id": row.product_id,
      "products.title": row.title,
      _sum: {
        "orders.subtotal": Number(row.subtotal_sum),
      },
    }));

    const resultRows = Array.isArray(response) ? response : response.data;

    expect(resultRows).toHaveLength(expected.length);
    expected.forEach((expectedRow, index) => {
      const actualRow = resultRows[index] as typeof expectedRow;
      expect(actualRow["orders.product_id"]).toBe(
        expectedRow["orders.product_id"]
      );
      expect(actualRow["products.title"]).toBe(expectedRow["products.title"]);
      expect(actualRow._sum["orders.subtotal"]).toBeCloseTo(
        expectedRow._sum["orders.subtotal"],
        4
      );
    });
  });

  test("countDistinct returns unique counts per group", async () => {
    const iql = {
      table: "orders",
      whereAndArray: [],
      operation: "groupBy" as const,
      operationParameters: {
        joins: null,
        groupBy: [{ type: "column", column: "orders.organisation_id" }],
        count: ["orders.id"],
        countDistinct: ["orders.product_id"],
        sum: null,
        min: null,
        max: null,
        avg: null,
        orderBy: {
          type: "aggregate" as const,
          function: "countDistinct",
          column: "orders.product_id",
          direction: "desc" as const,
        },
        limit: 10,
      },
    };

    const parsed = QuerySchema.parse(iql);
    const response = await groupBy(db, parsed);

    const expectedRows = await db
      .selectFrom("orders")
      .select([
        sql`organisation_id`.as("organisation_id"),
        sql<number>`COUNT(id)::int`.as("count_id"),
        sql<number>`COUNT(DISTINCT product_id)::int`.as("distinct_product_id"),
      ])
      .groupBy("organisation_id")
      .orderBy("distinct_product_id", "desc")
      .orderBy("organisation_id", "asc")
      .limit(10)
      .execute();

    const expected = expectedRows.map((row: any) => ({
      "orders.organisation_id": row.organisation_id,
      _count: { "orders.id": Number(row.count_id) },
      _countDistinct: {
        "orders.product_id": Number(row.distinct_product_id),
      },
    }));

    const resultRows = Array.isArray(response) ? response : response.data;

    expect(resultRows).toHaveLength(expected.length);
    expected.forEach((expectedRow, index) => {
      const actualRow = resultRows[index] as typeof expectedRow;
      expect(actualRow["orders.organisation_id"]).toBe(
        expectedRow["orders.organisation_id"]
      );
      expect(actualRow._count["orders.id"]).toBe(expectedRow._count["orders.id"]);
      expect(actualRow._countDistinct["orders.product_id"]).toBe(
        expectedRow._countDistinct["orders.product_id"]
      );
    });
  });

  test("supports HAVING on aggregates", async () => {
    const iql = {
      table: "orders",
      whereAndArray: [],
      operation: "groupBy" as const,
      operationParameters: {
        joins: null,
        groupBy: [{ type: "column", column: "orders.organisation_id" }],
        count: ["orders.id"],
        countDistinct: null,
        sum: ["orders.subtotal"],
        min: null,
        max: null,
        avg: null,
        orderBy: {
          type: "aggregate" as const,
          function: "count",
          column: "orders.id",
          direction: "desc" as const,
        },
        limit: 10,
        having: [
          { type: "aggregate", function: "count", column: "orders.id", operator: "gte", value: 2 },
          { type: "aggregate", function: "sum", column: "orders.subtotal", operator: "gt", value: 100 },
        ],
      },
    };

    const parsed = QuerySchema.parse(iql);
    const response = await groupBy(db, parsed);

    const expectedRows = await db
      .selectFrom("orders")
      .select([
        sql`organisation_id`.as("organisation_id"),
        sql<number>`COUNT(id)::int`.as("count_id"),
        sql<number>`SUM(subtotal)::float8`.as("sum_subtotal"),
      ])
      .groupBy("organisation_id")
      .having(sql`COUNT(id) >= 2`)
      .having(sql`SUM(subtotal) > 100`)
      .orderBy("count_id", "desc")
      .orderBy("organisation_id", "asc")
      .limit(10)
      .execute();

    const expected = expectedRows.map((row: any) => ({
      "orders.organisation_id": row.organisation_id,
      _count: { "orders.id": Number(row.count_id) },
      _sum: { "orders.subtotal": Number(row.sum_subtotal) },
    }));

    const resultRows = Array.isArray(response) ? response : response.data;

    expect(resultRows).toHaveLength(expected.length);
    expected.forEach((expectedRow, index) => {
      const actualRow = resultRows[index] as typeof expectedRow;
      expect(actualRow["orders.organisation_id"]).toBe(
        expectedRow["orders.organisation_id"]
      );
      expect(actualRow._count["orders.id"]).toBe(expectedRow._count["orders.id"]);
      expect(actualRow._sum["orders.subtotal"]).toBeCloseTo(
        expectedRow._sum["orders.subtotal"],
        4
      );
    });
  });

  test("supports HAVING on groupKey aliases", async () => {
    const iql = {
      table: "orders",
      whereAndArray: [],
      operation: "groupBy" as const,
      operationParameters: {
        joins: null,
        groupBy: [{ type: "column", column: "orders.organisation_id", alias: "org" }],
        count: ["orders.id"],
        countDistinct: null,
        sum: null,
        min: null,
        max: null,
        avg: null,
        orderBy: {
          type: "groupKey" as const,
          key: "org",
          direction: "asc" as const,
        },
        limit: 10,
        having: [
          { type: "groupKey", key: "org", operator: "not", value: null },
        ],
      },
    };

    const parsed = QuerySchema.parse(iql);
    const response = await groupBy(db, parsed);

    const expectedRows = await db
      .selectFrom("orders")
      .select([
        sql`organisation_id`.as("organisation_id"),
        sql<number>`COUNT(id)::int`.as("count_id"),
      ])
      .where(sql`organisation_id IS NOT NULL`)
      .groupBy("organisation_id")
      .orderBy("organisation_id", "asc")
      .limit(10)
      .execute();

    const expected = expectedRows.map((row: any) => ({
      org: row.organisation_id,
      _count: { "orders.id": Number(row.count_id) },
    }));

    const resultRows = Array.isArray(response) ? response : response.data;

    expect(resultRows).toHaveLength(expected.length);
    expected.forEach((expectedRow, index) => {
      const actualRow = resultRows[index] as typeof expectedRow;
      expect(actualRow["org"]).toBe(expectedRow["org"]);
      expect(actualRow._count["orders.id"]).toBe(expectedRow._count["orders.id"]);
    });
  });
});
