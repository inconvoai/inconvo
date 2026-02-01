// @ts-nocheck
import { sql, type Kysely } from "kysely";
import { loadTestEnv, getTestContext } from "../loadTestEnv";
describe("MySQL findMany Join Edge Cases", () => {
  let db: Kysely<any>;
  let QuerySchema: (typeof import("~/types/querySchema"))["QuerySchema"];
  let findMany: (typeof import("~/operations/findMany"))["findMany"];
  let clearSchemaCache: (typeof import("~/util/schemaCache"))["clearSchemaCache"];
  let clearAugmentedSchemaCache:
    | (typeof import("~/util/augmentedSchemaCache"))["clearAugmentedSchemaCache"]
    | undefined;
  let ctx: Awaited<ReturnType<typeof getTestContext>>;

  beforeAll(async () => {
    loadTestEnv("mysql");

    jest.resetModules();
    delete (globalThis as any).__INCONVO_KYSELY_DB__;

    ({ clearSchemaCache } = await import("~/util/schemaCache"));
    ({ clearAugmentedSchemaCache } = await import("~/util/augmentedSchemaCache"));

    QuerySchema = (await import("~/types/querySchema")).QuerySchema;
    findMany = (await import("~/operations/findMany")).findMany;
    const { getDb } = await import("~/dbConnection");
    db = await getDb();

    await clearSchemaCache();
    clearAugmentedSchemaCache?.();
    ctx = await getTestContext();
  });

  afterAll(async () => {
    if (db) await db.destroy();
    await clearSchemaCache?.();
    clearAugmentedSchemaCache?.();
  });

  test("deduplicates shared hop for multi-hop reviews join", async () => {
    const iql = {
      operation: "findMany" as const,
      table: "orders",
      tableConditions: null,
      whereAndArray: [],
      operationParameters: {
        select: {
          orders: ["id", "product_id"],
          "orders.product": ["title"],
          "orders.product.reviews": ["id"],
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
          {
            table: "reviews",
            name: "orders.product.reviews",
            path: [
              {
                source: ["orders.product_id"],
                target: ["products.id"],
              },
              {
                source: ["products.id"],
                target: ["reviews.product_id"],
              },
            ],
          },
        ],
        orderBy: {
          column: "id",
          direction: "asc" as const,
        },
        limit: 10,
      },
    };

    const parsed = QuerySchema.parse(iql);
    const response = await findMany(db, parsed, ctx);

    const expectedRows = await db
      .selectFrom("orders as o")
      .leftJoin("products as p", "p.id", "o.product_id")
      .leftJoin("reviews as r", "r.product_id", "p.id")
      .select([
        sql<number>`o.id`.as("id"),
        sql<number>`o.product_id`.as("product_id"),
        sql<string>`p.title`.as("product_title"),
        sql<number>`r.id`.as("review_id"),
      ])
      .orderBy("o.id", "asc")
      .limit(10)
      .execute();

    const expected = expectedRows.map((row: any) => ({
      orders_id: row.id === null ? row.id : Number(row.id),
      orders_product_id: row.product_id === null ? row.product_id : Number(row.product_id),
      "orders.product_title": row.product_title,
      "orders.product.reviews_id":
        row.review_id === null ? row.review_id : Number(row.review_id),
    }));

    expect(response.data).toEqual(expected);
  }, 15000);
});
