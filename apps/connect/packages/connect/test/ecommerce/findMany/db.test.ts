import { sql } from "drizzle-orm";
import { QuerySchema } from "~/types/querySchema";
import { findMany } from "~/operations/findMany/index";
import { getDb } from "~/dbConnection";

test("Which order recorded the highest subtotal and what product was sold?", async () => {
  const iql = {
    table: "orders",
    whereAndArray: [],
    operation: "findMany",
    operationParameters: {
      select: {
        orders: ["id", "subtotal"],
        "orders.products": ["title"],
      },
      joins: [
        {
          table: "products",
          name: "orders.products",
          path: [
            {
              source: ["orders.product_id"],
              target: ["products.id"],
            },
          ],
        },
      ],
      orderBy: {
        column: "subtotal",
        direction: "desc",
      },
      limit: 1,
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
        p.title AS product_title
      FROM orders o
      JOIN products p ON p.id = o.product_id
      ORDER BY o.subtotal DESC, o.id ASC
      LIMIT 1
    `
  );

  const expected = rows.map((row: any) => ({
    id: row.id,
    subtotal: Number(row.subtotal),
    "orders.products": row.product_title
      ? [
          {
            title: row.product_title,
          },
        ]
      : [],
  }));

  const resultRows = Array.isArray(response) ? response : response.data;

  expect(resultRows).toEqual(expected);
}, 10000);
