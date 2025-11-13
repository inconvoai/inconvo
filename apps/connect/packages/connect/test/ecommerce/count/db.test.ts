import { sql } from "drizzle-orm";
import { QuerySchema } from "~/types/querySchema";
import { count } from "~/operations/count";
import { getDb } from "~/dbConnection";

test("How many orders have we recorded?", async () => {
  const iql = {
    table: "orders",
    whereAndArray: [],
    operation: "count",
    operationParameters: {
      count: ["orders.id"],
      countDistinct: null,
    },
  };

  const parsedQuery = QuerySchema.parse(iql);
  const db = await getDb();
  const response = await count(db, parsedQuery);

  const { rows } = await db.execute(
    sql`SELECT COUNT(*)::int AS order_count FROM orders`
  );
  const [sqlResult] = rows as any[];

  const countResult = "data" in response ? response.data : response;

  expect(countResult).toEqual({
    _count: {
      "orders.id": Number(sqlResult.order_count),
    },
  });
});

test("How many orders reference a product and how many unique products appear?", async () => {
  const iql = {
    table: "orders",
    whereAndArray: [],
    operation: "count",
    operationParameters: {
      joins: [
        {
          table: "products",
          name: "prodAlias",
          path: [
            {
              source: ["orders.product_id"],
              target: ["products.id"],
            },
          ],
          joinType: "inner",
        },
      ],
      count: ["orders.product_id"],
      countDistinct: ["prodAlias.title"],
    },
  };

  const parsedQuery = QuerySchema.parse(iql);
  const db = await getDb();
  const response = await count(db, parsedQuery);

  const { rows } = await db.execute(
    sql`
      SELECT
        COUNT(p.id)::int AS total_product_rows,
        COUNT(DISTINCT p.title)::int AS distinct_product_titles
      FROM orders o
      JOIN products p ON p.id = o.product_id
    `
  );
  const [sqlResult] = rows as any[];

  const countResult = "data" in response ? response.data : response;

  expect(countResult).toEqual({
    _count: {
      "orders.product_id": Number(sqlResult.total_product_rows),
    },
    _countDistinct: {
      "prodAlias.title": Number(sqlResult.distinct_product_titles),
    },
  });
});

test("How many unique customers placed an order?", async () => {
  const iql = {
    table: "orders",
    whereAndArray: [],
    operation: "count",
    operationParameters: {
      count: null,
      countDistinct: ["orders.id"],
    },
  };

  const parsedQuery = QuerySchema.parse(iql);
  const db = await getDb();
  const response = await count(db, parsedQuery);

  const { rows } = await db.execute(
    sql`SELECT COUNT(DISTINCT id)::int AS distinct_order_ids FROM orders`
  );
  const [sqlResult] = rows as any[];

  const countResult = "data" in response ? response.data : response;

  expect(countResult).toEqual({
    _count: {},
    _countDistinct: {
      "orders.id": Number(sqlResult.distinct_order_ids),
    },
  });
});

test("Reject count operations without any metrics", () => {
  expect(() =>
    QuerySchema.parse({
      table: "orders",
      whereAndArray: [],
      operation: "count",
      operationParameters: {
        count: null,
        countDistinct: null,
      },
    })
  ).toThrow(/at least one metric/);
});
