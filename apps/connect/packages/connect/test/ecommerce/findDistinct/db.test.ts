import { sql } from "drizzle-orm";
import { QuerySchema } from "~/types/querySchema";
import { findDistinct } from "~/operations/findDistinct";
import { getDb } from "~/dbConnection";

test("Find the distinct cities our customers live in", async () => {
  const iql = {
    table: "users",
    whereAndArray: [],
    operation: "findDistinct",
    operationParameters: {
      column: "city",
    },
  };

  const parsedQuery = QuerySchema.parse(iql);
  const db = await getDb();
  const response = await findDistinct(db, parsedQuery);

  const { rows } = await db.execute(
    sql`
      SELECT DISTINCT city
      FROM users
      WHERE city IS NOT NULL
      ORDER BY city ASC
      LIMIT 500
    `
  );

  const responseRows = Array.isArray(response) ? response : response.data;

  const expectedCities = rows.map((row: any) => row.city).sort();
  const actualCities = responseRows.map((row: any) => row.city).sort();

  expect(actualCities).toEqual(expectedCities);
});
