import { sql } from "drizzle-orm";
import { QuerySchema } from "~/types/querySchema";
import { aggregate } from "~/operations/aggregate";
import { getDb } from "~/dbConnection";

test("What is the highest subtotal we have recorded on an order?", async () => {
  const iql = {
    table: "orders",
    whereAndArray: [],
    operation: "aggregate",
  operationParameters: {
      min: ["orders.subtotal"],
      max: ["orders.subtotal"],
      avg: ["orders.subtotal"],
      sum: ["orders.subtotal"],
      count: ["orders.subtotal"],
      median: null,
    },
  };

  const parsedQuery = QuerySchema.parse(iql);
  const db = await getDb();
  const response = await aggregate(db, parsedQuery);

  const { rows } = await db.execute(
    sql`
      SELECT
        AVG(subtotal)::float8       AS avg_subtotal,
        SUM(subtotal)::float8       AS sum_subtotal,
        MIN(subtotal)::float8       AS min_subtotal,
        MAX(subtotal)::float8       AS max_subtotal,
        COUNT(subtotal)::int        AS count_subtotal
      FROM orders
    `
  );
  const [sqlResult] = rows as any[];

  const aggregateResult = "data" in response ? response.data : response;

  expect(aggregateResult._avg["orders.subtotal"]).toBeCloseTo(
    Number(sqlResult.avg_subtotal),
    8
  );
  expect(aggregateResult._sum["orders.subtotal"]).toBeCloseTo(
    Number(sqlResult.sum_subtotal),
    4
  );
  expect(aggregateResult._min["orders.subtotal"]).toBe(
    Number(sqlResult.min_subtotal)
  );
  expect(aggregateResult._max["orders.subtotal"]).toBe(
    Number(sqlResult.max_subtotal)
  );
  expect(aggregateResult._count["orders.subtotal"]).toBe(
    Number(sqlResult.count_subtotal)
  );
});

test("What is the highest subtotal for an order priced over $100?", async () => {
  const iql = {
    table: "orders",
    whereAndArray: [
      {
        subtotal: {
          gt: 100,
        },
      },
    ],
    operation: "aggregate",
  operationParameters: {
      min: ["orders.subtotal"],
      max: ["orders.subtotal"],
      avg: ["orders.subtotal"],
      sum: ["orders.subtotal"],
      count: ["orders.subtotal"],
      median: null,
    },
  };

  const parsedQuery = QuerySchema.parse(iql);
  const db = await getDb();
  const response = await aggregate(db, parsedQuery);

  const { rows } = await db.execute(
    sql`
      SELECT
        AVG(subtotal)::float8       AS avg_subtotal,
        SUM(subtotal)::float8       AS sum_subtotal,
        MIN(subtotal)::float8       AS min_subtotal,
        MAX(subtotal)::float8       AS max_subtotal,
        COUNT(subtotal)::int        AS count_subtotal
      FROM orders
      WHERE subtotal > 100
    `
  );
  const [sqlResult] = rows as any[];

  const aggregateResult = "data" in response ? response.data : response;

  expect(aggregateResult._avg["orders.subtotal"]).toBeCloseTo(
    Number(sqlResult.avg_subtotal),
    8
  );
  expect(aggregateResult._sum["orders.subtotal"]).toBeCloseTo(
    Number(sqlResult.sum_subtotal),
    4
  );
  expect(aggregateResult._min["orders.subtotal"]).toBe(
    Number(sqlResult.min_subtotal)
  );
  expect(aggregateResult._max["orders.subtotal"]).toBe(
    Number(sqlResult.max_subtotal)
  );
  expect(aggregateResult._count["orders.subtotal"]).toBe(
    Number(sqlResult.count_subtotal)
  );
});

test("aggregates qualified column names with joins", async () => {
  const startDate = new Date("2025-10-27T00:00:00.000Z");
  const iql = {
    computedColumns: [],
    table: "orders",
    operation: "aggregate",
    operationParameters: {
      avg: null,
      count: null,
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
        organisation_id: {
          equals: 1,
        },
      },
      {
        AND: [
          {
            created_at: {
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

  const parsedQuery = QuerySchema.parse(iql);
  const db = await getDb();
  const response = await aggregate(db, parsedQuery);

  const { rows } = await db.execute(
    sql`
      SELECT
        SUM(orders.quantity)::float8 AS sum_orders_quantity
      FROM orders
      INNER JOIN products ON orders.product_id = products.id
      WHERE orders.organisation_id = 1
        AND orders.created_at >= ${startDate}
        AND products.title ILIKE '%MacBook%'
    `
  );
  const [sqlResult] = rows as any[];
  const expectedSum = Number(sqlResult?.sum_orders_quantity ?? 0);

  const aggregateResult = "data" in response ? response.data : response;
  expect(aggregateResult?._sum?.["orders.quantity"]).toBeCloseTo(
    expectedSum,
    6
  );
});
