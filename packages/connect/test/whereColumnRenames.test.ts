// @ts-nocheck
import { Kysely, PostgresDialect } from "kysely";
import { buildWhereConditions } from "~/operations/utils/whereConditionBuilder";

const schema = {
  tables: [
    {
      name: "users",
      columns: [
        { name: "id", type: "number" },
        { name: "first_name", type: "string", semanticName: "firstName" },
      ],
      columnRenameMap: {
        semanticToDb: { firstName: "first_name" },
        dbToSemantic: { first_name: "firstName" },
      },
      relations: [
        {
          name: "orders",
          isList: true,
          targetTable: "orders",
          sourceColumns: ["id"],
          targetColumns: ["user_id"],
        },
      ],
    },
    {
      name: "orders",
      columns: [
        { name: "user_id", type: "number" },
        { name: "total_amount", type: "number", semanticName: "totalAmount" },
      ],
      columnRenameMap: {
        semanticToDb: { totalAmount: "total_amount" },
        dbToSemantic: { total_amount: "totalAmount" },
      },
      relations: [],
    },
  ],
  databaseSchemas: null,
};

function createDb() {
  return new Kysely<any>({
    dialect: new PostgresDialect({ pool: {} as any }),
  });
}

describe("where conditions resolve semantic column renames", () => {
  test("qualified semantic column resolves to db column", () => {
    const db = createDb();
    const whereExpr = buildWhereConditions(
      [{ "users.firstName": { equals: "Alice" } }],
      "users",
      schema as any,
      "postgresql",
    );

    const compiled = db
      .selectFrom("users")
      .selectAll()
      .where(whereExpr as any)
      .compile();

    expect(compiled.sql).toContain(`"users"."first_name" = $1`);
    expect(compiled.parameters).toEqual(["Alice"]);
  });

  test("relation filter resolves nested semantic column to db column", () => {
    const db = createDb();
    const whereExpr = buildWhereConditions(
      [
        {
          orders: {
            some: {
              totalAmount: { gt: 100 },
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

    expect(compiled.sql).toContain(`"orders"."total_amount" > $1`);
    expect(compiled.parameters).toEqual([100]);
  });
});
