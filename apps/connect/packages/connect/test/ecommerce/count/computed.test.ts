import { sql } from "drizzle-orm";
import { QuerySchema } from "~/types/querySchema";
import { count } from "~/operations/count";
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

test("How many orders net more than $1,600 after tax and discount?", async () => {
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
    operation: "count",
    operationParameters: {
      count: ["orders.id", "orders.net_total"],
      countDistinct: null,
    },
  };

  const parsedQuery = QuerySchema.parse(iql);
  const db = await getDb();
  const response = await count(db, parsedQuery);

  const { rows } = await db.execute(
    sql`
      SELECT COUNT(*)::int AS matching_orders
      FROM orders
      WHERE (subtotal + tax - discount) > 1600
    `
  );
  const [sqlResult] = rows as any[];

  const countResult = "data" in response ? response.data : response;

  expect(countResult).toEqual({
    _count: {
      "orders.id": Number(sqlResult.matching_orders),
      "orders.net_total": Number(sqlResult.matching_orders),
    },
  });
});
