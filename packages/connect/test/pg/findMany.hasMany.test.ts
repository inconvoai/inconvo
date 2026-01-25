// @ts-nocheck
import type { Kysely } from "kysely";
import { loadTestEnv, getTestContext } from "../loadTestEnv";

describe("PostgreSQL findMany hasMany joins", () => {
  let db: Kysely<any>;
  let QuerySchema: (typeof import("~/types/querySchema"))["QuerySchema"];
  let findMany: (typeof import("~/operations/findMany"))["findMany"];
  let ctx: Awaited<ReturnType<typeof getTestContext>>;

  beforeAll(async () => {
    loadTestEnv("postgresql");

    jest.resetModules();
    delete (globalThis as any).__INCONVO_KYSELY_DB__;

    QuerySchema = (await import("~/types/querySchema")).QuerySchema;
    findMany = (await import("~/operations/findMany")).findMany;
    const { getDb } = await import("~/dbConnection");
    db = await getDb();
    ctx = await getTestContext();
  });

  afterAll(async () => {
    if (db) await db.destroy();
  });

  test("returns flat joined data for users with orders", async () => {
    // Simplified test - single join to orders
    const iql = {
      operation: "findMany" as const,
      table: "users",
      tableConditions: null,
      whereAndArray: [],
      operationParameters: {
        select: {
          users: ["id", "name", "created_at"],
          "users.orders": ["id", "subtotal", "created_at"],
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
        ],
        orderBy: {
          column: "created_at",
          direction: "desc" as const,
        },
        limit: 5,
      },
    };

    const parsed = QuerySchema.parse(iql);
    const response = await findMany(db, parsed, ctx);

    const rows = response.data ?? [];
    expect(rows.length).toBeGreaterThan(0);

    // Flat output format: {table}_{column} for base, {alias}_{column} for joins
    for (const row of rows) {
      // Base table columns
      expect(row.users_id).toBeDefined();
      expect(row.users_name).toBeDefined();
      expect(row.users_created_at).toBeDefined();

      // Joined orders columns (alias: users.orders - use bracket notation for dotted keys)
      expect(row["users.orders_id"]).toBeDefined();
      expect(row["users.orders_subtotal"]).toBeDefined();
      expect(row["users.orders_created_at"]).toBeDefined();
    }
  }, 20000);
});
