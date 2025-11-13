import { QuerySchema } from "~/types/querySchema";
import { findMany } from "~/operations/findMany";
import { getDb } from "~/dbConnection";

test("Inspect users with orders shape", async () => {
  const iql = {
    table: "users",
    whereAndArray: [],
    operation: "findMany",
    operationParameters: {
      select: {
        users: ["id", "name", "created_at"],
        "users.orders": ["id", "subtotal", "created_at"],
        "users.orders.products": ["title"],
      },
      joins: [
        {
          table: "orders",
          name: "users.orders",
          path: [
            {
              source: ["users.id"],
              target: ["orders.user_id"],
            },
          ],
        },
        {
          table: "products",
          name: "users.orders.products",
          path: [
            {
              source: ["users.id"],
              target: ["orders.user_id"],
            },
            {
              source: ["orders.product_id"],
              target: ["products.id"],
            },
          ],
        },
      ],
      orderBy: {
        column: "created_at",
        direction: "desc",
      },
      limit: 2,
    },
  };

  const parsedQuery = QuerySchema.parse(iql);
  const db = await getDb();
  const response = await findMany(db, parsedQuery);
  const resultRows = Array.isArray(response) ? response : response.data;
  expect(resultRows.length).toBeGreaterThan(0);

  for (const row of resultRows) {
    expect(row).toHaveProperty("id");
    expect(row).toHaveProperty("name");
    expect(row).toHaveProperty("created_at");
    const orders = row["users.orders.products"];
    expect(Array.isArray(orders)).toBe(true);
    for (const order of orders) {
      expect(order).toHaveProperty("id");
      expect(order).toHaveProperty("subtotal");
      expect(order).toHaveProperty("created_at");
      const products = order.products ?? [];
      expect(Array.isArray(products)).toBe(true);
      products.forEach((product: any) => {
        expect(product).toHaveProperty("title");
      });
    }
  }
}, 20000);
