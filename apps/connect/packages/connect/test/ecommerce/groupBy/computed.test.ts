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

test("Which product contributes the most net revenue when net total exceeds $1,000?", async () => {
  const iql = {
    table: "orders",
    computedColumns: [netTotalComputedColumn],
    whereAndArray: [
      {
        net_total: {
          gt: 1000,
        },
      },
    ],
    operation: "groupBy",
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
      sum: ["orders.net_total"],
      avg: null,
      min: null,
      max: null,
      orderBy: {
        type: "aggregate",
        function: "sum",
        column: "orders.net_total",
        direction: "desc",
      },
      limit: 1,
    },
  };

  const parsedQuery = QuerySchema.parse(iql);
  const db = await getDb();
  const response = await groupBy(db, parsedQuery);

  const { rows } = await db.execute(
    sql`
      SELECT
        o.product_id,
        p.title,
        SUM(o.subtotal + o.tax - o.discount)::float8 AS net_total_sum
      FROM orders o
      JOIN products p ON p.id = o.product_id
      WHERE (o.subtotal + o.tax - o.discount) > 1000
      GROUP BY o.product_id, p.title
      ORDER BY net_total_sum DESC, o.product_id ASC
      LIMIT 1
    `
  );
  const [sqlResult] = rows as any[];

  const resultRows = Array.isArray(response) ? response : response.data;

  expect(resultRows).toHaveLength(1);
  const [row] = resultRows as Array<{
    "orders.product_id": number;
    "products.title": string;
    _sum: Record<string, number>;
  }>;

  expect(row["orders.product_id"]).toBe(sqlResult.product_id);
  expect(row["products.title"]).toBe(sqlResult.title);
  expect(row._sum["orders.net_total"]).toBeCloseTo(
    Number(sqlResult.net_total_sum),
    6
  );
});
