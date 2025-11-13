import { sql } from "drizzle-orm";
import { QuerySchema } from "~/types/querySchema";
import { findMany } from "~/operations/findMany";
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

test("Which orders delivered the highest net totals and what products did they include?", async () => {
  const iql = {
    table: "orders",
    computedColumns: [netTotalComputedColumn],
    whereAndArray: [],
    operation: "findMany",
    operationParameters: {
      select: {
        orders: ["id", "subtotal", "net_total"],
        "orders.product": ["title"],
      },
      joins: [
        {
          table: "products",
          name: "orders.product",
          path: [
            {
              source: ["orders.product_id"],
              target: ["products.id"],
            },
          ],
        },
      ],
      orderBy: {
        column: "net_total",
        direction: "desc",
      },
      limit: 5,
    },
  };

  const parsedQuery = QuerySchema.parse(iql);
  const db = await getDb();
  const response = await findMany(db, parsedQuery);

  const { rows } = await db.execute(
    sql`
      SELECT
        o.id,
        o.subtotal,
        (o.subtotal + o.tax - o.discount)::float8 AS net_total,
        p.title AS product_title
      FROM orders o
      LEFT JOIN products p ON p.id = o.product_id
      ORDER BY net_total DESC, o.id ASC
      LIMIT 5
    `
  );

  const expected = rows.map((row: any) => ({
    id: row.id,
    subtotal: Number(row.subtotal),
    net_total: Number(row.net_total),
    "orders.product": row.product_title
      ? {
          title: row.product_title,
        }
      : null,
  }));

  const resultRows = Array.isArray(response) ? response : response.data;

  expect(resultRows).toEqual(expected);
});
