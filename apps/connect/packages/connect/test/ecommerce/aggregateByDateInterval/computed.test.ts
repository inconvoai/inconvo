import { sql } from "drizzle-orm";
import { QuerySchema } from "~/types/querySchema";
import { groupBy } from "~/operations/groupBy";
import { getDb } from "~/dbConnection";

const netTotalComputedColumn = {
  name: "net_total",
  table: {
    name: "orders" as const,
  },
  ast: {
    type: "operation" as const,
    operator: "-",
    operands: [
      {
        type: "operation" as const,
        operator: "+",
        operands: [
          {
            type: "column" as const,
            name: "subtotal",
          },
          {
            type: "column" as const,
            name: "tax",
          },
        ],
      },
      {
        type: "column" as const,
        name: "discount",
      },
    ],
  },
  type: "number" as const,
};

function parseAggregateCell(value: unknown): Record<string, number> {
  if (typeof value === "string") {
    return JSON.parse(value);
  }
  return (value as Record<string, number>) ?? {};
}

test("How many orders per month cleared more than $1,400 after tax and discount?", async () => {
  const iql = {
    table: "orders",
    computedColumns: [netTotalComputedColumn],
    whereAndArray: [
      {
        net_total: {
          gt: 1400,
        },
      },
    ],
    operation: "groupBy" as const,
    operationParameters: {
      joins: null,
      groupBy: [
        {
          type: "dateInterval" as const,
          column: "orders.created_at",
          interval: "month" as const,
          alias: "month_bucket",
        },
      ],
      count: ["orders.created_at"],
      sum: null,
      min: null,
      max: null,
      avg: null,
      orderBy: {
        type: "groupKey" as const,
        key: "month_bucket",
        direction: "asc" as const,
      },
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
        count: Number(parseAggregateCell(row._count)["orders.created_at"] ?? 0),
      },
    ])
  );

  const { rows: sqlRows } = await db.execute(
    sql`
      SELECT
        TO_CHAR(DATE_TRUNC('month', created_at), 'YYYY-MM') AS month_bucket,
        COUNT(*)::int                                       AS order_count
      FROM orders
      WHERE (subtotal + tax - discount) > 1400
      GROUP BY month_bucket
      ORDER BY month_bucket ASC
      LIMIT 12
    `
  );

  const expectedByMonth: Record<string, { count: number }> = Object.fromEntries(
    sqlRows.map((row: any) => [
      row.month_bucket,
      { count: Number(row.order_count) },
    ])
  );

  expect(byMonth).toEqual(expectedByMonth);
});

test("What was the average net total per month for orders over $1,400?", async () => {
  const iql = {
    table: "orders",
    computedColumns: [netTotalComputedColumn],
    whereAndArray: [
      {
        net_total: {
          gt: 1400,
        },
      },
    ],
    operation: "groupBy" as const,
    operationParameters: {
      joins: null,
      groupBy: [
        {
          type: "dateInterval" as const,
          column: "orders.created_at",
          interval: "month" as const,
          alias: "month_bucket",
        },
      ],
      count: null,
      sum: null,
      min: null,
      max: null,
      avg: ["orders.net_total"],
      orderBy: {
        type: "groupKey" as const,
        key: "month_bucket",
        direction: "asc" as const,
      },
      limit: 12,
    },
  };

  const parsedQuery = QuerySchema.parse(iql);
  const db = await getDb();
  const response = await groupBy(db, parsedQuery);

  const rows = "data" in response ? response.data : response;
  const byMonth: Record<string, { avg: number }> = Object.fromEntries(
    rows.map((row: any) => [
      row.month_bucket,
      {
        avg: Number(parseAggregateCell(row._avg)["orders.net_total"] ?? 0),
      },
    ])
  );

  const { rows: sqlRows } = await db.execute(
    sql`
      SELECT
        TO_CHAR(DATE_TRUNC('month', created_at), 'YYYY-MM') AS month_bucket,
        AVG(subtotal + tax - discount)::float8             AS avg_net_total
      FROM orders
      WHERE (subtotal + tax - discount) > 1400
      GROUP BY month_bucket
      ORDER BY month_bucket ASC
      LIMIT 12
    `
  );

  const expectedByMonth: Record<string, { avg: number }> = Object.fromEntries(
    sqlRows.map((row: any) => [
      row.month_bucket,
      { avg: Number(row.avg_net_total) },
    ])
  );

  expect(Object.keys(byMonth)).toEqual(Object.keys(expectedByMonth));
  for (const [month, expected] of Object.entries(expectedByMonth)) {
    expect(byMonth[month]).toBeDefined();
    expect(byMonth[month]!.avg).toBeCloseTo(expected.avg, 4);
  }
});
