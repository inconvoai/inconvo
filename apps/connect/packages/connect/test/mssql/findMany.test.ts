// @ts-nocheck
import { sql } from "kysely";
import type { Kysely } from "kysely";
import { loadTestEnv } from "../loadTestEnv";

describe("MSSQL findMany Operation", () => {
  let db: Kysely<any>;
  let QuerySchema: typeof import("~/types/querySchema")["QuerySchema"];
  let findMany: typeof import("~/operations/findMany")["findMany"];

  beforeAll(async () => {
    loadTestEnv("mssql");

    jest.resetModules();
    delete (globalThis as any).__INCONVO_KYSELY_DB__;

    QuerySchema = (await import("~/types/querySchema")).QuerySchema;
    findMany = (await import("~/operations/findMany")).findMany;
    const { getDb } = await import("~/dbConnection");
    db = await getDb();
  });

  afterAll(async () => {
    if (db) await db.destroy();
  });

  test("Which order recorded the highest subtotal and what product was sold?", async () => {
    const iql = {
      operation: "findMany" as const,
      table: "orders",
      whereAndArray: [
        {
          revenue: {
            gt: 0,
          },
        },
      ],
      operationParameters: {
        select: {
          orders: ["id", "revenue", "created_at"],
          "orders.product": ["title"],
        },
        joins: [
          {
            table: "products",
            name: "orders.product",
            path: [
              {
                source: ["orders.product_id"],
                target: ["products.id"],
              },
            ],
          },
        ],
        orderBy: {
          column: "revenue",
          direction: "desc" as const,
        },
        limit: 1,
      },
    };

    const parsed = QuerySchema.parse(iql);
    const response = await findMany(db, parsed);

    const revenueExpr = sql`((o.subtotal - o.discount) + o.tax) * o.quantity`;

    const expectedRow = await db
      .selectFrom("orders as o")
      .innerJoin("products as p", "p.id", "o.product_id")
      .select([
        sql<number>`o.id`.as("id"),
        sql<number>`${revenueExpr}`.as("revenue"),
        sql<string>`p.title`.as("product_title"),
        sql`o.created_at`.as("created_at"),
      ])
      .where(revenueExpr, ">", 0)
      .orderBy(revenueExpr, "desc")
      .executeTakeFirst();

    const expected = expectedRow
      ? [
          {
            id: Number(expectedRow.id),
            revenue: Number(expectedRow.revenue),
            created_at: expectedRow.created_at,
            "orders.product": [
              {
                title: expectedRow.product_title,
              },
            ],
          },
        ]
      : [];

    const normalizeRows = (rows: any[]) =>
      rows.map((row) => ({
        ...row,
        created_at:
          row.created_at && typeof row.created_at.toISOString === "function"
            ? row.created_at.toISOString()
            : row.created_at,
      }));

    expect(normalizeRows(response.data)).toEqual(normalizeRows(expected));
  }, 15000);
});
