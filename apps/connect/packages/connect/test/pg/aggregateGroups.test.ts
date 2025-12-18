// @ts-nocheck
import { sql, type Kysely } from "kysely";
import { loadTestEnv } from "../loadTestEnv";

describe("PostgreSQL aggregateGroups Operation", () => {
  let db: Kysely<any>;
  let QuerySchema: typeof import("~/types/querySchema")["QuerySchema"];
  let aggregateGroups: typeof import("~/operations/aggregateGroups")["aggregateGroups"];

  beforeAll(async () => {
    loadTestEnv("postgresql");

    jest.resetModules();
    delete (globalThis as any).__INCONVO_KYSELY_DB__;

    QuerySchema = (await import("~/types/querySchema")).QuerySchema;
    aggregateGroups = (await import("~/operations/aggregateGroups")).aggregateGroups;
    const { getDb } = await import("~/dbConnection");
    db = await getDb();
  });

  afterAll(async () => {
    await db?.destroy?.();
  });

  test("counts groups after HAVING", async () => {
    const iql = {
      table: "orders",
      whereAndArray: [],
      operation: "aggregateGroups" as const,
      operationParameters: {
        joins: null,
        groupBy: [{ type: "column", column: "orders.user_id" }],
        having: [
          { type: "aggregate", function: "count", column: "orders.id", operator: "gt", value: 2 },
        ],
        aggregates: { groupCount: true },
      },
    };

    const parsed = QuerySchema.parse(iql);
    const response = await aggregateGroups(db, parsed);

    const expectedGroups = await db
      .selectFrom("orders")
      .select(sql`user_id`.as("user_id"))
      .groupBy("user_id")
      .having(sql`COUNT(id) > 2`)
      .execute();

    expect(response.data?.groupCount).toBe(expectedGroups.length);
  });

  test("reduces per-group counts and sums", async () => {
    const iql = {
      table: "orders",
      whereAndArray: [],
      operation: "aggregateGroups" as const,
      operationParameters: {
        joins: null,
        groupBy: [{ type: "column", column: "orders.user_id" }],
        having: [
          { type: "aggregate", function: "count", column: "orders.id", operator: "gt", value: 1 },
        ],
        aggregates: { groupCount: true, count: ["orders.id"], sum: ["orders.subtotal"] },
        reducers: { count: ["sum", "max"], sum: ["sum", "avg"] },
      },
    };

    const parsed = QuerySchema.parse(iql);
    const response = await aggregateGroups(db, parsed);

    const grouped = await db
      .selectFrom("orders")
      .select([
        sql`user_id`.as("user_id"),
        sql<number>`COUNT(id)`.as("count_id"),
        sql<number>`SUM(subtotal)`.as("sum_subtotal"),
      ])
      .groupBy("user_id")
      .having(sql`COUNT(id) > 1`)
      .execute();

    const expectedGroupCount = grouped.length;
    const totalOrderCount = grouped.reduce(
      (acc, row) => acc + Number(row.count_id ?? 0),
      0
    );
    const maxOrderCount = grouped.reduce(
      (acc, row) => Math.max(acc, Number(row.count_id ?? 0)),
      0
    );
    const totalRevenue = grouped.reduce(
      (acc, row) => acc + Number(row.sum_subtotal ?? 0),
      0
    );
    const avgRevenue = grouped.length ? totalRevenue / grouped.length : 0;

    const data = response.data;
    expect(data?.groupCount).toBe(expectedGroupCount);
    expect(data?._count?.["orders.id"]?.sum).toBe(totalOrderCount);
    expect(data?._count?.["orders.id"]?.max).toBe(maxOrderCount);
    expect(data?._sum?.["orders.subtotal"]?.sum).toBeCloseTo(totalRevenue, 4);
    expect(data?._sum?.["orders.subtotal"]?.avg).toBeCloseTo(avgRevenue, 4);
  });
});
