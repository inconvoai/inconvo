// @ts-nocheck
import type { Kysely } from "kysely";
import { loadTestEnv } from "../loadTestEnv";

describe("BigQuery simple smoke tests", () => {
  let db: Kysely<any>;
  let getCachedSchema: typeof import("~/util/schemaCache")["getCachedSchema"];
  let clearSchemaCache: typeof import("~/util/schemaCache")["clearSchemaCache"];

  beforeAll(async () => {
    jest.setTimeout(120000);
    loadTestEnv("bigquery");

    jest.resetModules();
    delete (globalThis as any).__INCONVO_KYSELY_DB__;

    ({ getCachedSchema, clearSchemaCache } = await import(
      "~/util/schemaCache"
    ));
    await clearSchemaCache();

    const { getDb } = await import("~/dbConnection");
    db = await getDb();
  });

  afterAll(async () => {
    await db?.destroy?.();
    await clearSchemaCache?.();
  });

  it("builds schema without errors", async () => {
    await clearSchemaCache();
    const schema = await getCachedSchema();
    expect(schema).toBeDefined();
    expect(Array.isArray(schema.tables)).toBe(true);
    const tableNames = (schema.tables ?? []).map((table) => table.name);
    expect(tableNames).toContain("orders");
  });

  it("executes a simple select query", async () => {
    const rows = await db.selectFrom("users").selectAll().limit(5).execute();
    expect(Array.isArray(rows)).toBe(true);
  });

  it("executes a count query", async () => {
    const result = await db
      .selectFrom("users")
      .select((eb) => eb.fn.countAll().as("count"))
      .executeTakeFirst();
    expect(result).toBeDefined();
    expect(Number(result?.count ?? 0)).toBeGreaterThanOrEqual(0);
  });

  it("executes aggregation queries", async () => {
    const result = await db
      .selectFrom("orders")
      .select((eb) => [
        eb.fn.sum<number | null>("quantity").as("total_quantity"),
        eb.fn.avg<number | null>("subtotal").as("avg_subtotal"),
        eb.fn.count("id").as("order_count"),
      ])
      .executeTakeFirst();

    expect(result).toBeDefined();
    expect(result?.order_count).toBeDefined();
  });
});