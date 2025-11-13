import { sql } from "drizzle-orm";
import { QuerySchema } from "~/types/querySchema";
import { groupBy } from "~/operations/groupBy";
import { getDb } from "~/dbConnection";

test("Which products drive the highest order subtotal?", async () => {
  const iql = {
    table: "orders",
    whereAndArray: [],
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
      sum: ["orders.subtotal"],
      min: null,
      max: null,
      avg: null,
      orderBy: {
        type: "aggregate",
        function: "sum",
        column: "orders.subtotal",
        direction: "desc",
      },
      limit: 10,
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
        SUM(o.subtotal)::float8 AS subtotal_sum
      FROM orders o
      JOIN products p ON p.id = o.product_id
      GROUP BY o.product_id, p.title
      ORDER BY subtotal_sum DESC, o.product_id ASC
      LIMIT 10
    `
  );

  const expected: Array<{
    "orders.product_id": number;
    "products.title": string;
    _sum: Record<string, number>;
  }> = rows.map((row: any) => ({
    _sum: {
      "orders.subtotal": Number(row.subtotal_sum),
    },
    "orders.product_id": row.product_id,
    "products.title": row.title,
  }));

  const resultRows = Array.isArray(response) ? response : response.data;

  expect(resultRows).toHaveLength(expected.length);
  expected.forEach((expectedRow, index) => {
    const actualRow = resultRows[index] as typeof expectedRow;
    expect(actualRow["orders.product_id"]).toBe(
      expectedRow["orders.product_id"]
    );
    expect(actualRow["products.title"]).toBe(expectedRow["products.title"]);
    expect(actualRow._sum["orders.subtotal"]).toBeCloseTo(
      expectedRow._sum["orders.subtotal"],
      4
    );
  });
});
