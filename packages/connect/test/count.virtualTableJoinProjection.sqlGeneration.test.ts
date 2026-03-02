// @ts-nocheck
import { Kysely, MysqlDialect, MssqlDialect, PostgresDialect } from "kysely";
import { BigQueryDialect } from "~/dialects/bigquery";

const virtualSql = `SELECT orders.*, organisations.name as org_name
FROM orders
JOIN organisations
  ON organisations.id = orders.organisation_id;`;

type TestDialect =
  | "postgresql"
  | "mysql"
  | "mssql"
  | "bigquery"
  | "redshift";

function createDb(dialect: TestDialect) {
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

  if (dialect === "mssql") {
    return new Kysely<any>({
      dialect: new MssqlDialect({
        tarn: {
          Pool: class {
            constructor(_opts: unknown) {}
          },
          options: {
            min: 0,
            max: 1,
            acquireTimeoutMillis: 1000,
            createTimeoutMillis: 1000,
            destroyTimeoutMillis: 1000,
            idleTimeoutMillis: 1000,
            propagateCreateError: true,
          },
        } as any,
        tedious: {
          connectionFactory: async () => ({}),
          resetConnectionOnRelease: false,
        } as any,
      }),
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

function buildSchema(dialect: TestDialect) {
  return {
    tables: [
      {
        name: "orders_with_org",
        columns: [
          { name: "id", type: "number" },
          { name: "organisation_id", type: "number" },
          { name: "org_name", type: "string" },
        ],
        virtualTable: {
          sql: virtualSql,
          dialect,
          sourceColumns: [
            { sourceName: "id", name: "id" },
            { sourceName: "organisation_id", name: "organisation_id" },
            { sourceName: "org_name", name: "org_name" },
          ],
        },
      },
    ],
    databaseSchemas: null,
  };
}

describe("count SQL virtual table join projection support", () => {
  let countOperation: (typeof import("~/operations/count"))["count"];
  const executeWithLoggingMock = jest.fn(async (query: any) => ({
    rows: [{}],
    compiled: query.compile(),
  }));

  beforeAll(async () => {
    await jest.unstable_mockModule("~/operations/utils/executeWithLogging", () => ({
      executeWithLogging: executeWithLoggingMock,
    }));
    ({ count: countOperation } = await import("~/operations/count"));
  });

  beforeEach(() => {
    executeWithLoggingMock.mockClear();
  });

  test.each([
    { dialect: "postgresql" },
    { dialect: "mysql" },
    { dialect: "mssql" },
    { dialect: "bigquery" },
    { dialect: "redshift" },
  ] as const)(
    "builds count SQL from join-based virtual table source for $dialect",
    async ({ dialect }) => {
      const db = createDb(dialect);
      const response = await countOperation(
        db,
        {
          table: "orders_with_org",
          tableConditions: null,
          whereAndArray: [],
          operation: "count",
          operationParameters: {
            count: ["orders_with_org.id"],
            countDistinct: ["orders_with_org.org_name"],
          },
        } as any,
        {
          schema: buildSchema(dialect),
          dialect,
        },
      );

      const normalizedSql = response.query.sql.toLowerCase();
      expect(normalizedSql).toContain("from (select");
      expect(normalizedSql).toContain("from orders");
      expect(normalizedSql).toContain("join organisations");
      expect(normalizedSql).toContain("organisations.name as org_name");
      expect(normalizedSql).toContain("__inconvo_vt_src.id as id");
      expect(normalizedSql).toContain("__inconvo_vt_src.organisation_id as organisation_id");
      expect(normalizedSql).toContain("__inconvo_vt_src.org_name as org_name");
      expect(executeWithLoggingMock).toHaveBeenCalledTimes(1);
    },
  );
});
