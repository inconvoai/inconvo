// @ts-nocheck
import { loadTestEnv } from "../loadTestEnv";

describe("BigQuery buildSchema relations", () => {
  beforeAll(() => {
    jest.setTimeout(120000);
    loadTestEnv("bigquery");
  });

  beforeEach(() => {
    jest.resetModules();
    delete (globalThis as any).__INCONVO_KYSELY_DB__;
  });

  test("captures forward and reverse relations for orders", async () => {
    const { clearSchemaCache } = await import("~/util/schemaCache");
    await clearSchemaCache();

    const { buildSchema } = await import("~/util/buildSchema");
    const schema = await buildSchema();

    const ordersTable = schema.tables.find((table) => table.name === "orders");
    expect(ordersTable).toBeDefined();
    expect(ordersTable?.relations).toBeDefined();

    const hasUserRelation = ordersTable?.relations?.some((relation) => {
      return (
        !relation.isList &&
        relation.targetTable === "users" &&
        (relation.sourceColumns ?? []).includes("user_id") &&
        (relation.targetColumns ?? []).includes("id")
      );
    });

    const hasProductRelation = ordersTable?.relations?.some((relation) => {
      return (
        !relation.isList &&
        relation.targetTable === "products" &&
        (relation.sourceColumns ?? []).includes("product_id") &&
        (relation.targetColumns ?? []).includes("id")
      );
    });

    expect(hasUserRelation).toBe(true);
    expect(hasProductRelation).toBe(true);

    const usersTable = schema.tables.find((table) => table.name === "users");
    expect(usersTable).toBeDefined();

    const hasOrdersReverseRelation = usersTable?.relations?.some((relation) => {
      return (
        relation.isList &&
        relation.targetTable === "orders" &&
        (relation.sourceColumns ?? []).includes("id") &&
        (relation.targetColumns ?? []).includes("user_id")
      );
    });

    expect(hasOrdersReverseRelation).toBe(true);

    await clearSchemaCache();
  });
});
