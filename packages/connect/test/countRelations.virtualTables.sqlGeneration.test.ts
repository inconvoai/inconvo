// @ts-nocheck
import { Kysely, PostgresDialect } from "kysely";

const schema = {
  tables: [
    {
      name: "virtual_users",
      columns: [{ name: "id", type: "number" }],
      relations: [
        {
          name: "orders",
          isList: true,
          targetTable: "orders",
          sourceColumns: ["id"],
          targetColumns: ["user_id"],
        },
      ],
      virtualTable: {
        sql: "SELECT id FROM users",
        dialect: "postgresql",
        sourceColumns: [{ sourceName: "id", name: "id" }],
      },
    },
    {
      name: "orders",
      columns: [
        { name: "id", type: "number" },
        { name: "user_id", type: "number" },
      ],
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

describe("countRelations SQL virtual table support", () => {
  let countRelationsOperation: (typeof import("~/operations/countRelations"))["countRelations"];
  const executeWithLoggingMock = jest.fn(async (query: any) => ({
    rows: [],
    compiled: query.compile(),
  }));

  beforeAll(async () => {
    await jest.unstable_mockModule("~/operations/utils/executeWithLogging", () => ({
      executeWithLogging: executeWithLoggingMock,
    }));
    ({ countRelations: countRelationsOperation } = await import(
      "~/operations/countRelations"
    ));
  });

  beforeEach(() => {
    executeWithLoggingMock.mockClear();
  });

  test("builds countRelations SQL from a virtual base source", async () => {
    const db = createDb();

    const response = await countRelationsOperation(
      db,
      {
        table: "virtual_users",
        tableConditions: null,
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
                  source: ["virtual_users.id"],
                  target: ["orders.user_id"],
                },
              ],
            },
          ],
          relationsToCount: [{ name: "orders", distinct: null }],
          orderBy: null,
          limit: 10,
        },
      } as any,
      {
        schema,
        dialect: "postgresql",
      },
    );

    const normalizedSql = response.query.sql.toLowerCase();
    expect(normalizedSql).toContain("from (select");
    expect(response.query.sql).toContain('as "virtual_users"');
    expect(response.query.sql).toContain('"virtual_users"."id"');
    expect(executeWithLoggingMock).toHaveBeenCalledTimes(1);
  });
});
