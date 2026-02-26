// @ts-nocheck
import { Kysely, PostgresDialect } from "kysely";
import { buildWhereConditions } from "~/operations/utils/whereConditionBuilder";

const schema = {
  tables: [
    {
      name: "users",
      columns: [{ name: "id", type: "number" }],
      relations: [
        {
          name: "recentOrders",
          isList: true,
          targetTable: "recent_orders",
          sourceColumns: ["id"],
          targetColumns: ["user_id"],
        },
      ],
    },
    {
      name: "recent_orders",
      columns: [
        { name: "user_id", type: "number" },
        { name: "total_amount", type: "number" },
      ],
      relations: [],
      virtualTable: {
        sql: "SELECT user_id, total_amount FROM orders",
        dialect: "postgresql",
        sourceColumns: [
          { sourceName: "user_id", name: "user_id" },
          { sourceName: "total_amount", name: "total_amount" },
        ],
      },
    },
  ],
  databaseSchemas: null,
};

function createDb() {
  return new Kysely<any>({
    dialect: new PostgresDialect({ pool: {} as any }),
  });
}

describe("where relation filters with virtual targets", () => {
  test("uses the virtual target subquery in EXISTS filters", () => {
    const db = createDb();
    const whereExpr = buildWhereConditions(
      [
        {
          recentOrders: {
            some: {
              total_amount: { gt: 100 },
            },
          },
        },
      ],
      "users",
      schema as any,
      "postgresql",
    );

    const compiled = db
      .selectFrom("users")
      .selectAll()
      .where(whereExpr as any)
      .compile();

    const normalizedSql = compiled.sql.toLowerCase();
    expect(normalizedSql).toContain("exists (select 1 from (select");
    expect(compiled.sql).toContain('as "recent_orders"');
    expect(compiled.sql).toContain('"recent_orders"."user_id" = "users"."id"');
    expect(compiled.parameters).toEqual([100]);
  });
});
