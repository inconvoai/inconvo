// @ts-nocheck
import type { Kysely } from "kysely";
import { loadTestEnv } from "../loadTestEnv";

describe("BigQuery findDistinctByEditDistance Operation", () => {
  let db: Kysely<any>;
  let QuerySchema: (typeof import("~/types/querySchema"))["QuerySchema"];
  let findDistinctByEditDistance: (typeof import("~/operations/findDistinctByEditDistance"))["findDistinctByEditDistance"];

  beforeAll(async () => {
    jest.setTimeout(120000);
    loadTestEnv("bigquery");

    jest.resetModules();
    delete (globalThis as any).__INCONVO_KYSELY_DB__;

    QuerySchema = (await import("~/types/querySchema")).QuerySchema;
    findDistinctByEditDistance = (
      await import("~/operations/findDistinctByEditDistance")
    ).findDistinctByEditDistance;
    const { getDb } = await import("~/dbConnection");
    db = await getDb();
  });

  afterAll(async () => {
    await db?.destroy?.();
  });

  test("findDistinctByEditDistance for product names similar to model", async () => {
    const iql = {
      operation: "findDistinctByEditDistance" as const,
      table: "products",
      tableConditions: null,
      whereAndArray: [],
      operationParameters: {
        column: "title",
        compareString: "Model",
      },
    };

    const parsed = QuerySchema.parse(iql);
    const response = await findDistinctByEditDistance(db, parsed);
    expect(response).toHaveProperty("data");
    expect(Array.isArray(response.data)).toBe(true);
    expect(response.data[0]).toContain("Model");
  });
});
