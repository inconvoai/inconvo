// @ts-nocheck
import type { Kysely } from "kysely";
import { sql } from "kysely";
import { loadTestEnv } from "../loadTestEnv";

describe("BigQuery findDistinct Operation", () => {
  let db: Kysely<any>;
  let QuerySchema: typeof import("~/types/querySchema")["QuerySchema"];
  let findDistinct: typeof import("~/operations/findDistinct")["findDistinct"];

  beforeAll(async () => {
    jest.setTimeout(120000);
    loadTestEnv("bigquery");

    jest.resetModules();
    delete (globalThis as any).__INCONVO_KYSELY_DB__;

    QuerySchema = (await import("~/types/querySchema")).QuerySchema;
    findDistinct = (await import("~/operations/findDistinct")).findDistinct;
    const { getDb } = await import("~/dbConnection");
    db = await getDb();
  });

  afterAll(async () => {
    await db?.destroy?.();
  });

  test("Find the distinct cities our customers live in", async () => {
    const iql = {
      table: "users",
      whereAndArray: [],
      operation: "findDistinct" as const,
      operationParameters: {
        column: "city",
      },
    };

    const parsed = QuerySchema.parse(iql);
    const response = await findDistinct(db, parsed);

    const { rows: sqlRows } = await sql`
      SELECT DISTINCT city
      FROM users
      WHERE city IS NOT NULL
      ORDER BY city
      LIMIT 500
    `.execute(db);

    const expectedCities = sqlRows
      .map((row: any) => row.city)
      .filter((city): city is string => city !== null)
      .sort();

    const resultRows = Array.isArray(response) ? response : response.data;
    const actualCities = resultRows
      .map((row: any) => row.city)
      .filter((city): city is string => city !== null)
      .sort();

    expect(actualCities).toEqual(expectedCities);
  });
});