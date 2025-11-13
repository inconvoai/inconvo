import { sql } from "drizzle-orm";
import { QuerySchema } from "~/types/querySchema";
import { countRelations } from "~/operations/countRelations";
import { getDb } from "~/dbConnection";

test("Which customer has placed the most orders?", async () => {
  const iql = {
    table: "users",
    whereAndArray: [],
    operation: "countRelations",
    operationParameters: {
      columns: ["id", "organisation_id"],
      joins: [
        {
          table: "orders",
          name: "orders",
          path: [
            {
              source: ["users.id"],
              target: ["orders.user_id"],
            },
          ],
        },
      ],
      relationsToCount: [
        {
          name: "orders",
          distinct: null,
        },
      ],
      orderBy: {
        name: "orders",
        direction: "desc",
      },
      limit: 1,
    },
  };

  const parsedQuery = QuerySchema.parse(iql);
  const db = await getDb();
  const response = await countRelations(db, parsedQuery);

  const { rows } = await db.execute(
    sql`
      SELECT
        u.id,
        u.organisation_id,
        COUNT(o.*)::int AS order_count
      FROM users u
      JOIN orders o ON o.user_id = u.id
      GROUP BY u.id, u.organisation_id
      ORDER BY order_count DESC, u.id ASC
      LIMIT 1
    `
  );
  const [sqlResult] = rows as any[];

  const resultRows = Array.isArray(response) ? response : response.data;

  expect(resultRows).toEqual([
    {
      id: sqlResult.id,
      organisation_id: sqlResult.organisation_id,
      orders_count: Number(sqlResult.order_count),
    },
  ]);
}, 10000);

test("How many distinct orders has each user placed?", async () => {
  const iql = {
    table: "users",
    whereAndArray: [],
    operation: "countRelations",
    operationParameters: {
      columns: ["id"],
      joins: [
        {
          table: "orders",
          name: "orders",
          path: [
            {
              source: ["users.id"],
              target: ["orders.user_id"],
            },
          ],
          joinType: "left",
        },
      ],
      relationsToCount: [
        {
          name: "orders",
          distinct: "orders.id",
        },
      ],
      orderBy: null,
      limit: 5,
    },
  };

  const parsedQuery = QuerySchema.parse(iql);
  const db = await getDb();
  const response = await countRelations(db, parsedQuery);

  const { rows } = await db.execute(
    sql`
      SELECT
        u.id,
        COUNT(DISTINCT o.id)::int AS order_count
      FROM users u
      LEFT JOIN orders o ON o.user_id = u.id
      GROUP BY u.id
      ORDER BY u.id ASC
      LIMIT 5
    `
  );

  const alias = "orders_distinctCount";
  const expected = rows.map((row: any) => ({
    id: row.id,
    [alias]: Number(row.order_count),
  }));

  const resultRows = Array.isArray(response) ? response : response.data;

  expect(resultRows).toEqual(expected);
});
