import { sql } from "drizzle-orm";
import { QuerySchema } from "~/types/querySchema";
import { groupBy } from "~/operations/groupBy";
import { getDb } from "~/dbConnection";

function parseAggregateCell(value: unknown): Record<string, number> {
  if (typeof value === "string") {
    return JSON.parse(value);
  }
  return (value as Record<string, number>) ?? {};
}

test("How many orders were placed in each month?", async () => {
  const iql = {
    table: "orders",
    whereAndArray: [],
    operation: "groupBy",
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
      sum: null,
      min: null,
      max: null,
      avg: null,
      orderBy: { type: "groupKey", key: "month_bucket", direction: "asc" },
      limit: 12,
    },
  };

  const parsedQuery = QuerySchema.parse(iql);
  const db = await getDb();
  const response = await groupBy(db, parsedQuery);

  const rows = "data" in response ? response.data : response;
  const byMonth: Record<string, { count: number }> = Object.fromEntries(
    rows.map((row: any) => [
      row.month_bucket,
      {
        count: Number(
          parseAggregateCell(row._count)["orders.created_at"] ?? 0
        ),
      },
    ])
  );

  const { rows: sqlRows } = await db.execute(
    sql`
      SELECT
        TO_CHAR(DATE_TRUNC('month', created_at), 'YYYY-MM') AS month_bucket,
        COUNT(*)::int                                       AS order_count
      FROM orders
      GROUP BY month_bucket
      ORDER BY month_bucket ASC
      LIMIT 12
    `
  );

  const expectedByMonth = Object.fromEntries(
    sqlRows.map((row: any) => [
      row.month_bucket,
      { count: Number(row.order_count) },
    ])
  );

  expect(byMonth).toEqual(expectedByMonth);
});
