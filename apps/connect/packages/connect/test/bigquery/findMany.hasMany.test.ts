// @ts-nocheck
import type { Kysely } from "kysely";
import { loadTestEnv } from "../loadTestEnv";

describe("BigQuery findMany hasMany joins", () => {
  let db: Kysely<any>;
  let QuerySchema: typeof import("~/types/querySchema")["QuerySchema"];
  let findMany: typeof import("~/operations/findMany")["findMany"];

  beforeAll(async () => {
    jest.setTimeout(120000);
    loadTestEnv("bigquery");

    jest.resetModules();
    delete (globalThis as any).__INCONVO_KYSELY_DB__;

    QuerySchema = (await import("~/types/querySchema")).QuerySchema;
    findMany = (await import("~/operations/findMany")).findMany;
    const { getDb } = await import("~/dbConnection");
    db = await getDb();
  });

  afterAll(async () => {
    await db?.destroy?.();
  });

  test(
    "returns orders (hasMany) nested under users with product titles",
    async () => {
      const iql = {
        operation: "findMany" as const,
        table: "users",
        whereAndArray: [],
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
            direction: "desc" as const,
          },
          limit: 3,
        },
      };

      const parsed = QuerySchema.parse(iql);
      const response = await findMany(db, parsed);

      const rows = response.data ?? [];
      expect(rows.length).toBeGreaterThan(0);
      rows.forEach((row) => {
        expect(row).toHaveProperty("id");
        expect(row).toHaveProperty("name");
        const ordersRaw = row["users.orders.products"];
        expect(ordersRaw).toBeDefined();
        const orders = Array.isArray(ordersRaw) ? ordersRaw : [ordersRaw];
        orders.forEach((order) => {
          expect(order).toHaveProperty("id");
          expect(order).toHaveProperty("subtotal");
          expect(order).toHaveProperty("created_at");
          const productsRaw = order.products ?? [];
          const products = Array.isArray(productsRaw) ? productsRaw : [productsRaw];
          products.forEach((product) => {
            expect(product).toHaveProperty("title");
          });
        });
      });
    },
    150000
  );
});