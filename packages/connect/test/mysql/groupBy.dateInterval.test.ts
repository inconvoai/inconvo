// @ts-nocheck
import { sql, type Kysely } from "kysely";
import { loadTestEnv, getTestContext } from "../loadTestEnv";

function parseAggregateCell(value: unknown): Record<string, number> {
  if (typeof value === "string") {
    return JSON.parse(value);
  }
  return (value as Record<string, number>) ?? {};
}

describe("MySQL groupBy dateInterval buckets", () => {
  let db: Kysely<any>;
  let QuerySchema: (typeof import("~/types/querySchema"))["QuerySchema"];
  let groupBy: (typeof import("~/operations/groupBy"))["groupBy"];
  let ctx: Awaited<ReturnType<typeof getTestContext>>;

  beforeAll(async () => {
    loadTestEnv("mysql");

    jest.resetModules();
    delete (globalThis as any).__INCONVO_KYSELY_DB__;

    QuerySchema = (await import("~/types/querySchema")).QuerySchema;
    groupBy = (await import("~/operations/groupBy")).groupBy;
    const { getDb } = await import("~/dbConnection");
    db = await getDb();
    ctx = await getTestContext();
  });

  afterAll(async () => {
    if (db) await db.destroy();
  });

  test("How many orders were placed in each month?", async () => {
    const iql = {
      table: "orders",
      tableConditions: null,
      whereAndArray: [],
      operation: "groupBy" as const,
      operationParameters: {
        joins: null,
        groupBy: [
          {
            type: "dateInterval",
            column: "orders.created_at",
            interval: "month",
            alias: "month_bucket",
          },
        ],
        count: ["orders.created_at"],
        countDistinct: null,
        sum: null,
        min: null,
        max: null,
        avg: null,
        orderBy: { type: "groupKey", key: "month_bucket", direction: "asc" },
        limit: 12,
      },
    };

    const parsed = QuerySchema.parse(iql);
    const response = await groupBy(db, parsed, ctx);

    const rows = "data" in response ? response.data : response;
    const byMonth: Record<string, { count: number }> = Object.fromEntries(
      rows.map((row: any) => [
        row.month_bucket,
        {
          count:
            parseAggregateCell(row._count)["orders.created_at"] !== undefined
              ? Number(parseAggregateCell(row._count)["orders.created_at"])
              : 0,
        },
      ]),
    );

    const { rows: sqlRows } = await sql`
      SELECT
        DATE_FORMAT(created_at, '%Y-%m') AS month_bucket,
        COUNT(*)                        AS order_count
      FROM orders
      GROUP BY DATE_FORMAT(created_at, '%Y-%m')
      ORDER BY month_bucket ASC
      LIMIT 12
    `.execute(db);

    const expectedByMonth = Object.fromEntries(
      sqlRows.map((row: any) => [
        row.month_bucket,
        { count: Number(row.order_count) },
      ]),
    );

    expect(byMonth).toEqual(expectedByMonth);
  });
});
