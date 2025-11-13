import { sql } from "drizzle-orm";
import { QuerySchema } from "~/types/querySchema";
import { countRelations } from "~/operations/countRelations";
import { getDb } from "~/dbConnection";

const inventoryValueColumn = {
  name: "inventory_value",
  table: {
    name: "products" as const,
  },
  ast: {
    type: "operation" as const,
    operator: "*",
    operands: [
      {
        type: "column" as const,
        name: "price",
      },
      {
        type: "column" as const,
        name: "stock_level",
      },
    ],
  },
  type: "number" as const,
};

test("Which product with a large inventory value has attracted the most orders?", async () => {
  const iql = {
    table: "products",
    computedColumns: [inventoryValueColumn],
    whereAndArray: [
      {
        inventory_value: {
          gt: 40000,
        },
      },
    ],
    operation: "countRelations",
    operationParameters: {
      columns: ["id", "title", "inventory_value"],
      joins: [
        {
          table: "orders",
          name: "orders",
          path: [
            {
              source: ["products.id"],
              target: ["orders.product_id"],
            },
          ],
        },
      ],
      relationsToCount: [
        {
          name: "orders",
          distinct: null,
        },
      ],
      orderBy: {
        name: "orders",
        direction: "desc",
      },
      limit: 1,
    },
  };

  const parsedQuery = QuerySchema.parse(iql);
  const db = await getDb();
  const response = await countRelations(db, parsedQuery);

  const { rows } = await db.execute(
    sql`
      SELECT
        p.id,
        p.title,
        (p.price * p.stock_level)::float8 AS inventory_value,
        COUNT(o.*)::int                  AS order_count
      FROM products p
      JOIN orders o ON o.product_id = p.id
      WHERE (p.price * p.stock_level) > 40000
      GROUP BY p.id, p.title, inventory_value
      ORDER BY order_count DESC, p.id ASC
      LIMIT 1
    `
  );
  const [sqlResult] = rows as any[];

  const resultRows = Array.isArray(response) ? response : response.data;

  expect(resultRows).toEqual([
    {
      id: sqlResult.id,
      title: sqlResult.title,
      inventory_value: Number(sqlResult.inventory_value),
      orders_count: Number(sqlResult.order_count),
    },
  ]);
}, 10000);
