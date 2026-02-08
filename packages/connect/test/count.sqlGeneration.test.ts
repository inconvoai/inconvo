// @ts-nocheck
import { Kysely, MysqlDialect, PostgresDialect } from "kysely";
import { BigQueryDialect } from "~/dialects/bigquery";

const schema = {
  tables: [
    {
      name: "orders",
      columns: [
        { name: "id'o", type: "number" },
        { name: "total's", type: "number" },
      ],
    },
  ],
  databaseSchemas: null,
};

function createDb(dialect: "postgresql" | "mysql" | "bigquery" | "redshift") {
  if (dialect === "postgresql" || dialect === "redshift") {
    return new Kysely<any>({
      dialect: new PostgresDialect({ pool: {} as any }),
    });
  }

  if (dialect === "mysql") {
    return new Kysely<any>({
      dialect: new MysqlDialect({ pool: {} as any }),
    });
  }

  return new Kysely<any>({
    dialect: new BigQueryDialect({
      projectId: "test-project",
      dataset: "test_dataset",
      client: {} as any,
    }),
  });
}

describe("count SQL JSON builder parity", () => {
  let countOperation: (typeof import("~/operations/count"))["count"];
  const executeWithLoggingMock = jest.fn(async (query: any) => ({
    rows: [{ _count: {}, _countDistinct: {} }],
    compiled: query.compile(),
  }));

  beforeAll(async () => {
    await jest.unstable_mockModule("~/operations/utils/executeWithLogging", () => ({
      executeWithLogging: executeWithLoggingMock,
    }));
    ({ count: countOperation } = await import("~/operations/count"));
  });

  test.each([
    { dialect: "postgresql", expectedFragment: "json_build_object(" },
    { dialect: "mysql", expectedFragment: "json_object(" },
    { dialect: "bigquery", expectedFragment: "json_object(" },
    { dialect: "redshift", expectedFragment: "object(" },
  ] as const)(
    "uses the shared JSON-object builder for $dialect count SQL",
    async ({ dialect, expectedFragment }) => {
      const db = createDb(dialect);
      const response = await countOperation(
        db,
        {
          table: "orders",
          tableConditions: null,
          whereAndArray: [],
          operation: "count",
          operationParameters: {
            count: ["orders.id'o"],
            countDistinct: ["orders.total's"],
          },
        } as any,
        {
          schema,
          dialect,
        },
      );

      const normalizedSql = response.query.sql.toLowerCase();
      expect(normalizedSql).toContain(expectedFragment);
      expect(response.query.sql).toMatch(/'orders\.id''o'/);
      expect(response.query.sql).toMatch(/'orders\.total''s'/);

      if (dialect === "redshift") {
        expect(normalizedSql).not.toContain("json_build_object(");
      }
    },
  );
});
