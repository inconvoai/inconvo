import { sql } from "drizzle-orm";
import { QuerySchema } from "~/types/querySchema";
import { findDistinct } from "~/operations/findDistinct";
import { getDb } from "~/dbConnection";

const netTotalComputedColumn = {
  name: "net_total",
  table: {
    name: "orders" as const,
  },
  ast: {
    type: "operation" as const,
    operator: "-",
    operands: [
      {
        type: "operation" as const,
        operator: "+",
        operands: [
          {
            type: "column" as const,
            name: "subtotal",
          },
          {
            type: "column" as const,
            name: "tax",
          },
        ],
      },
      {
        type: "column" as const,
        name: "discount",
      },
    ],
  },
  type: "number" as const,
};

test("Find the organisations with orders exceeding $1,500 net total", async () => {
  const iql = {
    table: "orders",
    computedColumns: [netTotalComputedColumn],
    whereAndArray: [
      {
        net_total: {
          gt: 1500,
        },
      },
    ],
    operation: "findDistinct",
    operationParameters: {
      column: "organisation_id",
    },
  };

  const parsedQuery = QuerySchema.parse(iql);
  const db = await getDb();
  const response = await findDistinct(db, parsedQuery);

  const { rows } = await db.execute(
    sql`
      SELECT DISTINCT organisation_id
      FROM orders
      WHERE (subtotal + tax - discount) > 1500
      ORDER BY organisation_id ASC
    `
  );

  const responseRows = Array.isArray(response) ? response : response.data;

  const expectedOrgIds = rows.map((row: any) => row.organisation_id).sort();
  const actualOrgIds = responseRows
    .map((row: any) => row.organisation_id)
    .sort();

  expect(actualOrgIds).toEqual(expectedOrgIds);
});
