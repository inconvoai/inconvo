// @ts-nocheck
import { Kysely } from "kysely";
import { getCachedSchema } from "../../src/util/schemaCache";
import { loadTestEnv } from "../loadTestEnv";

describe("PostgreSQL Simple Query Test", () => {
  let db: Kysely<any>;

  beforeAll(async () => {
    loadTestEnv("postgresql");

    jest.resetModules();
    delete (globalThis as any).__INCONVO_KYSELY_DB__;
    const { getDb } = await import("~/dbConnection");
    db = await getDb();
  });

  afterAll(async () => {
    await db.destroy();
  });

  it("should get schema without errors", async () => {
    const schema = await getCachedSchema();
    expect(schema).toBeDefined();
    expect(schema.tables).toBeDefined();
  });

  it("should execute a simple select query", async () => {
    // Use LIMIT for PostgreSQL
    const query = db.selectFrom("users").selectAll().limit(5);
    const result = await query.execute();
    expect(Array.isArray(result)).toBe(true);
  });

  it("should execute a count query", async () => {
    const result = await db
      .selectFrom("users")
      .select(db.fn.count("id").as("count"))
      .executeTakeFirst();
    expect(result).toBeDefined();
  });

  it("should execute aggregation queries", async () => {
    const result = await db
      .selectFrom("orders")
      .select([
        db.fn.sum("quantity").as("total_quantity"),
        db.fn.avg("subtotal").as("avg_subtotal"),
        db.fn.count("id").as("order_count"),
      ])
      .executeTakeFirst();
    expect(result).toBeDefined();
  });
});
