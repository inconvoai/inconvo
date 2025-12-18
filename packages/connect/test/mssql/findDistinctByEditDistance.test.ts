// @ts-nocheck
import { getDb } from "~/dbConnection";
import { findDistinctByEditDistance } from "~/operations/findDistinctByEditDistance";
import { Kysely } from "kysely";
import { loadTestEnv } from "../loadTestEnv";

describe("MSSQL findDistinctByEditDistance Operation", () => {
  let db: Kysely<any>;

  beforeAll(async () => {
    loadTestEnv("mssql");

    db = await getDb();
  });

  afterAll(async () => {
    if (db) await db.destroy();
  });

  test("findDistinctByEditDistance for product names similar to model", async () => {
    const query = {
      operation: "findDistinctByEditDistance" as const,
      table: "products",
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
