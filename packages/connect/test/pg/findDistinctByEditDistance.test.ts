// @ts-nocheck
import { Kysely } from "kysely";
import { loadTestEnv } from "../loadTestEnv";

describe("PostgreSQL findDistinctByEditDistance Operation", () => {
  let db: Kysely<any>;
  let findDistinctByEditDistance: (typeof import("~/operations/findDistinctByEditDistance"))["findDistinctByEditDistance"];

  beforeAll(async () => {
    loadTestEnv("postgresql");

    jest.resetModules();
    delete (globalThis as any).__INCONVO_KYSELY_DB__;
    findDistinctByEditDistance = (
      await import("~/operations/findDistinctByEditDistance")
    ).findDistinctByEditDistance;
    const { getDb } = await import("~/dbConnection");
    db = await getDb();
  });

  afterAll(async () => {
    if (db) await db.destroy();
  });

  test("findDistinctByEditDistance for product names similar to model", async () => {
    const query = {
      operation: "findDistinctByEditDistance" as const,
      table: "products",
      tableConditions: null,
      whereAndArray: [],
      operationParameters: {
        column: "title",
        compareString: "Model",
      },
    };

    const response = await findDistinctByEditDistance(db, query);
    expect(response).toHaveProperty("data");
    expect(Array.isArray(response.data)).toBe(true);
    expect(response.data[0]).toContain("Model");
  });
});
