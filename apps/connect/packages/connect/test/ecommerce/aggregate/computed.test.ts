import { sql } from "drizzle-orm";
import { QuerySchema } from "~/types/querySchema";
import { aggregate } from "~/operations/aggregate";
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

test("What is our average order total including tax and discount?", async () => {
  const iql = {
    table: "orders",
    computedColumns: [netTotalComputedColumn],
    whereAndArray: [],
    operation: "aggregate",
    operationParameters: {
      min: ["orders.net_total"],
      max: ["orders.net_total"],
      avg: ["orders.net_total"],
      sum: ["orders.net_total"],
      count: ["orders.net_total"],
      median: null,
    },
  };

  const parsedQuery = QuerySchema.parse(iql);
  const db = await getDb();
  const response = await aggregate(db, parsedQuery);

  const { rows } = await db.execute(
    sql`
      SELECT
        AVG(subtotal + tax - discount)::float8 AS avg_net_total,
        SUM(subtotal + tax - discount)::float8 AS sum_net_total,
        MIN(subtotal + tax - discount)::float8 AS min_net_total,
        MAX(subtotal + tax - discount)::float8 AS max_net_total,
        COUNT(*)::int                          AS count_orders
      FROM orders
    `
  );
  const [sqlResult] = rows as any[];

  const aggregateResult = "data" in response ? response.data : response;

  expect(aggregateResult._avg["orders.net_total"]).toBeCloseTo(
    Number(sqlResult.avg_net_total),
    10
  );
  expect(aggregateResult._sum["orders.net_total"]).toBeCloseTo(
    Number(sqlResult.sum_net_total),
    4
  );
  expect(aggregateResult._min["orders.net_total"]).toBe(
    Number(sqlResult.min_net_total)
  );
  expect(aggregateResult._max["orders.net_total"]).toBe(
    Number(sqlResult.max_net_total)
  );
  expect(aggregateResult._count["orders.net_total"]).toBe(
    Number(sqlResult.count_orders)
  );
});

test("How many orders cleared more than $1,600 after tax and discount?", async () => {
  const iql = {
    table: "orders",
    computedColumns: [netTotalComputedColumn],
    whereAndArray: [
      {
        net_total: {
          gt: 1600,
        },
      },
    ],
    operation: "aggregate",
    operationParameters: {
      min: ["orders.net_total"],
      max: ["orders.net_total"],
      avg: ["orders.net_total"],
      sum: ["orders.net_total"],
      count: ["orders.net_total"],
      median: null,
    },
  };

  const parsedQuery = QuerySchema.parse(iql);
  const db = await getDb();
  const response = await aggregate(db, parsedQuery);

  const { rows } = await db.execute(
    sql`
      SELECT
        AVG(subtotal + tax - discount)::float8 AS avg_net_total,
        SUM(subtotal + tax - discount)::float8 AS sum_net_total,
        MIN(subtotal + tax - discount)::float8 AS min_net_total,
        MAX(subtotal + tax - discount)::float8 AS max_net_total,
        COUNT(*)::int                          AS count_orders
      FROM orders
      WHERE (subtotal + tax - discount) > 1600
    `
  );
  const [sqlResult] = rows as any[];

  const aggregateResult = "data" in response ? response.data : response;

  expect(aggregateResult._avg["orders.net_total"]).toBeCloseTo(
    Number(sqlResult.avg_net_total),
    10
  );
  expect(aggregateResult._sum["orders.net_total"]).toBeCloseTo(
    Number(sqlResult.sum_net_total),
    4
  );
  expect(aggregateResult._min["orders.net_total"]).toBe(
    Number(sqlResult.min_net_total)
  );
  expect(aggregateResult._max["orders.net_total"]).toBe(
    Number(sqlResult.max_net_total)
  );
  expect(aggregateResult._count["orders.net_total"]).toBe(
    Number(sqlResult.count_orders)
  );
});
