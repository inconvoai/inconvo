// @ts-nocheck
import { sql, type Kysely } from "kysely";
import { loadTestEnv, getTestContext } from "../loadTestEnv";

const virtualSql = `SELECT orders.*, organisations.name as org_name
FROM orders
JOIN organisations
  ON organisations.id = orders.organisation_id;`;

const DIALECT_LABELS: Record<string, string> = {
  postgresql: "PostgreSQL",
  mysql: "MySQL",
  mssql: "MSSQL",
  bigquery: "BigQuery",
};

interface DialectTestOptions {
  dialect: string;
  expectedSqlBuilder: (db: Kysely<any>) => ReturnType<Kysely<any>["selectFrom"]>;
  beforeAllSetup?: () => void;
}

export function createVirtualTableJoinProjectionTests({
  dialect,
  expectedSqlBuilder,
  beforeAllSetup,
}: DialectTestOptions) {
  const label = DIALECT_LABELS[dialect] ?? dialect;

  describe(`${label} count Operation (virtual table join projection)`, () => {
    let db: Kysely<any>;
    let QuerySchema: (typeof import("~/types/querySchema"))["QuerySchema"];
    let count: (typeof import("~/operations/count"))["count"];
    let ctx: Awaited<ReturnType<typeof getTestContext>>;

    beforeAll(async () => {
      beforeAllSetup?.();
      loadTestEnv(dialect);

      jest.resetModules();
      delete (globalThis as any).__INCONVO_KYSELY_DB__;

      QuerySchema = (await import("~/types/querySchema")).QuerySchema;
      count = (await import("~/operations/count")).count;
      const { getDb } = await import("~/dbConnection");
      db = await getDb();
      ctx = await getTestContext();
    });

    afterAll(async () => {
      if (db) await db.destroy();
    });

    test("executes count/countDistinct over a virtual table built from orders + organisations join", async () => {
      expect(ctx.schema.tables.some((t: any) => t.name === "orders")).toBe(true);
      expect(ctx.schema.tables.some((t: any) => t.name === "organisations")).toBe(
        true,
      );

      const virtualSchema = {
        ...ctx.schema,
        tables: [
          ...ctx.schema.tables,
          {
            name: "orders_with_org",
            columns: [
              { name: "id", type: "number" },
              { name: "organisation_id", type: "number" },
              { name: "org_name", type: "string" },
            ],
            relations: [],
            computedColumns: [],
            columnConversions: [],
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
      };

      const iql = {
        table: "orders_with_org",
        tableConditions: null,
        whereAndArray: [],
        operation: "count" as const,
        operationParameters: {
          count: ["orders_with_org.id"],
          countDistinct: ["orders_with_org.org_name"],
        },
      };

      const parsed = QuerySchema.parse(iql);
      const response = await count(db, parsed, {
        ...ctx,
        schema: virtualSchema,
      });

      const expectedRows = await expectedSqlBuilder(db).execute();

      const expected = expectedRows[0] ?? { total_rows: 0, distinct_org_names: 0 };
      const result = response.data ?? response;

      expect(result).toEqual({
        _count: {
          "orders_with_org.id": Number(expected.total_rows ?? 0),
        },
        _countDistinct: {
          "orders_with_org.org_name": Number(expected.distinct_org_names ?? 0),
        },
      });
    }, 10000);
  });
}
