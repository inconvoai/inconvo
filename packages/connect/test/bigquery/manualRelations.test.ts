// @ts-nocheck
/**
 * Manual Relation Filter Tests for BigQuery
 *
 * BigQuery deliberately uses unqualified table names in WHERE clauses and
 * EXISTS subqueries (the dataset is set at connection level). These tests
 * verify that manual relations without explicit targetSchema do NOT produce
 * schema-qualified table names in the generated SQL.
 */
import { sql } from "kysely";
import type { Kysely } from "kysely";
import { loadTestEnv, getTestContext } from "../loadTestEnv";
import type { SchemaResponse } from "../../src/types/types";
import type { OperationContext } from "../../src/operations/types";

describe("BigQuery Manual Relation Filters", () => {
  let db: Kysely<any>;
  let QuerySchema: (typeof import("~/types/querySchema"))["QuerySchema"];
  let findMany: (typeof import("~/operations/findMany"))["findMany"];
  let clearSchemaCache: (typeof import("~/util/schemaCache"))["clearSchemaCache"];

  let manualCtx: OperationContext;

  beforeAll(async () => {
    jest.setTimeout(120000);
    loadTestEnv("bigquery");

    jest.resetModules();
    delete (globalThis as any).__INCONVO_KYSELY_DB__;

    ({ clearSchemaCache } = await import("~/util/schemaCache"));
    await clearSchemaCache();

    QuerySchema = (await import("~/types/querySchema")).QuerySchema;
    findMany = (await import("~/operations/findMany")).findMany;
    const { getDb } = await import("~/dbConnection");
    db = await getDb();

    const dataset = process.env.INCONVO_BIGQUERY_DATASET;

    // Build schema with manual relations — no targetSchema set
    const manualSchema: SchemaResponse = {
      tables: [
        {
          name: "users",
          schema: dataset,
          columns: [
            { name: "id", type: "number" },
            { name: "name", type: "string" },
            { name: "email", type: "string" },
          ],
          relations: [
            {
              name: "orders",
              isList: true,
              targetTable: "orders",
              // targetSchema intentionally omitted — tests BigQuery handling
              sourceColumns: ["id"],
              targetColumns: ["user_id"],
              source: "MANUAL",
            },
          ],
        },
        {
          name: "orders",
          schema: dataset,
          columns: [
            { name: "id", type: "number" },
            { name: "user_id", type: "number" },
            { name: "subtotal", type: "number" },
            { name: "created_at", type: "DateTime" },
          ],
          relations: [
            {
              name: "user",
              isList: false,
              targetTable: "users",
              // targetSchema intentionally omitted — tests BigQuery handling
              sourceColumns: ["user_id"],
              targetColumns: ["id"],
              source: "MANUAL",
            },
          ],
        },
      ],
      databaseSchemas: dataset ? [dataset] : [],
    };

    manualCtx = { schema: manualSchema, dialect: "bigquery" };
  });

  afterAll(async () => {
    await db?.destroy?.();
    await clearSchemaCache?.();
  });

  it("does NOT schema-qualify target table in EXISTS for manual relation filter", async () => {
    const dataset = process.env.INCONVO_BIGQUERY_DATASET;

    const iql = {
      operation: "findMany" as const,
      table: "users",
      tableConditions: null,
      whereAndArray: [
        {
          AND: [
            {
              orders: {
                some: {},
              },
            },
          ],
        },
      ],
      operationParameters: {
        select: { users: ["id", "name"] },
        orderBy: { column: "id", direction: "asc" as const },
        limit: 10,
      },
    };

    const parsed = QuerySchema.parse(iql);
    const response = await findMany(db, parsed, manualCtx);

    // BigQuery should NOT schema-qualify table names in EXISTS subqueries
    if (dataset) {
      expect(response.query.sql).not.toContain(`${dataset}.orders`);
    }
    // The SQL should contain an EXISTS subquery with bare table name
    expect(response.query.sql).toContain("EXISTS");
    expect(response.data).toBeDefined();
  });

  it("executes manual 'is' relation filter without schema qualification", async () => {
    const dataset = process.env.INCONVO_BIGQUERY_DATASET;

    const iql = {
      operation: "findMany" as const,
      table: "orders",
      tableConditions: null,
      whereAndArray: [
        {
          AND: [
            {
              user: {
                is: {
                  name: { not: null },
                },
              },
            },
          ],
        },
      ],
      operationParameters: {
        select: { orders: ["id", "subtotal"] },
        orderBy: { column: "id", direction: "asc" as const },
        limit: 10,
      },
    };

    const parsed = QuerySchema.parse(iql);
    const response = await findMany(db, parsed, manualCtx);

    // BigQuery should NOT schema-qualify table names in EXISTS subqueries
    if (dataset) {
      expect(response.query.sql).not.toContain(`${dataset}.users`);
    }
    expect(response.query.sql).toContain("EXISTS");
    expect(response.data).toBeDefined();
  });
});
