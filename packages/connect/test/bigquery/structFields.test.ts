// @ts-nocheck
import { sql, type Kysely } from "kysely";
import { loadTestEnv, getTestContext } from "../loadTestEnv";

describe("BigQuery STRUCT Field Support", () => {
  let db: Kysely<any>;
  let QuerySchema: (typeof import("~/types/querySchema"))["QuerySchema"];
  let findMany: (typeof import("~/operations/findMany"))["findMany"];
  let groupBy: (typeof import("~/operations/groupBy"))["groupBy"];
  let count: (typeof import("~/operations/count"))["count"];
  let getCachedSchema: (typeof import("~/util/schemaCache"))["getCachedSchema"];
  let clearSchemaCache: (typeof import("~/util/schemaCache"))["clearSchemaCache"];
  let ctx: Awaited<ReturnType<typeof getTestContext>>;

  beforeAll(async () => {
    jest.setTimeout(120000);
    loadTestEnv("bigquery");

    jest.resetModules();
    delete (globalThis as any).__INCONVO_KYSELY_DB__;

    ({ getCachedSchema, clearSchemaCache } =
      await import("~/util/schemaCache"));
    await clearSchemaCache();

    QuerySchema = (await import("~/types/querySchema")).QuerySchema;
    findMany = (await import("~/operations/findMany")).findMany;
    groupBy = (await import("~/operations/groupBy")).groupBy;
    count = (await import("~/operations/count")).count;
    const { getDb } = await import("~/dbConnection");
    db = await getDb();
    ctx = await getTestContext();
  });

  afterAll(async () => {
    await db?.destroy?.();
    await clearSchemaCache?.();
  });

  describe("Schema Introspection", () => {
    it("includes events table in schema", async () => {
      await clearSchemaCache();
      const schema = await getCachedSchema();
      expect(schema).toBeDefined();

      const tableNames = (schema.tables ?? []).map((table) => table.name);
      expect(tableNames).toContain("events");
    });

    it("surfaces STRUCT fields as columns with # separator", async () => {
      await clearSchemaCache();
      const schema = await getCachedSchema();

      const eventsTable = schema.tables.find((t) => t.name === "events");
      expect(eventsTable).toBeDefined();

      const columnNames = eventsTable.columns.map((c) => c.name);

      // Check that STRUCT field columns exist with # separator
      expect(columnNames).toContain("properties#payment_method");
      expect(columnNames).toContain("properties#status");
      expect(columnNames).toContain("properties#currency");
      expect(columnNames).toContain("raw_properties#payment_method");
      expect(columnNames).toContain("raw_properties#currency");
    });

    it("sets correct metadata on STRUCT field columns", async () => {
      await clearSchemaCache();
      const schema = await getCachedSchema();

      const eventsTable = schema.tables.find((t) => t.name === "events");
      const paymentMethodCol = eventsTable.columns.find(
        (c) => c.name === "properties#payment_method",
      );

      expect(paymentMethodCol).toBeDefined();
      expect(paymentMethodCol.isStructField).toBe(true);
      expect(paymentMethodCol.structParent).toBe("properties");
      expect(paymentMethodCol.structFieldPath).toBe("payment_method");
      expect(paymentMethodCol.type).toBe("string");
    });

    it("excludes raw STRUCT columns from schema", async () => {
      await clearSchemaCache();
      const schema = await getCachedSchema();

      const eventsTable = schema.tables.find((t) => t.name === "events");
      const columnNames = eventsTable.columns.map((c) => c.name);

      // The parent STRUCT columns should not be included directly
      // Only the nested fields with # notation should be present
      const structParentColumns = columnNames.filter(
        (name) => name === "properties" || name === "raw_properties",
      );
      expect(structParentColumns.length).toBe(0);
    });
  });

  describe("findMany with STRUCT fields", () => {
    it("can select STRUCT fields", async () => {
      const iql = {
        operation: "findMany" as const,
        table: "events",
        tableConditions: null,
        whereAndArray: [],
        operationParameters: {
          select: {
            events: [
              "type",
              "properties#payment_method",
              "properties#currency",
              "properties#status",
            ],
          },
          orderBy: {
            column: "type",
            direction: "asc" as const,
          },
          limit: 10,
        },
      };

      const parsed = QuerySchema.parse(iql);
      const response = await findMany(db, parsed, ctx);

      expect(response.data).toBeDefined();
      expect(Array.isArray(response.data)).toBe(true);
      expect(response.data.length).toBeGreaterThan(0);

      // Verify the STRUCT fields are returned (new flat format: table_column)
      const firstRow = response.data[0];
      expect(firstRow.events_type).toBeDefined();
      expect(firstRow["events_properties#payment_method"]).toBeDefined();
      expect(firstRow["events_properties#currency"]).toBeDefined();
    });

    it("can filter on STRUCT fields", async () => {
      const iql = {
        operation: "findMany" as const,
        table: "events",
        tableConditions: null,
        whereAndArray: [
          {
            "events.properties#payment_method": {
              equals: "card",
            },
          },
        ],
        operationParameters: {
          select: {
            events: ["type", "properties#payment_method"],
          },
          orderBy: {
            column: "type",
            direction: "asc" as const,
          },
          limit: 10,
        },
      };

      const parsed = QuerySchema.parse(iql);
      const response = await findMany(db, parsed, ctx);

      expect(response.data).toBeDefined();
      expect(response.data.length).toBeGreaterThan(0);

      // All returned rows should have payment_method = 'card' (new flat format: table_column)
      for (const row of response.data) {
        expect(row["events_properties#payment_method"]).toBe("card");
      }
    });

    it("can order by STRUCT fields", async () => {
      const iql = {
        operation: "findMany" as const,
        table: "events",
        tableConditions: null,
        whereAndArray: [],
        operationParameters: {
          select: {
            events: ["type", "properties#currency"],
          },
          orderBy: {
            column: "properties#currency",
            direction: "asc" as const,
          },
          limit: 10,
        },
      };

      const parsed = QuerySchema.parse(iql);
      const response = await findMany(db, parsed, ctx);

      expect(response.data).toBeDefined();
      expect(response.data.length).toBeGreaterThan(0);
    });

    it("can select and order by numeric STRUCT fields", async () => {
      const iql = {
        operation: "findMany" as const,
        table: "events",
        tableConditions: null,
        whereAndArray: [],
        operationParameters: {
          joins: null,
          limit: 1,
          orderBy: {
            column: "properties#total_price",
            direction: "desc" as const,
          },
          select: {
            events: [
              "type",
              "timestamp",
              "properties#total_price",
              "properties#currency",
              "properties#purchase_id",
            ],
          },
        },
      };

      const parsed = QuerySchema.parse(iql);
      const response = await findMany(db, parsed, ctx);

      expect(response.data).toBeDefined();
      expect(Array.isArray(response.data)).toBe(true);
      expect(response.data.length).toBe(1);

      // New flat format: table_column
      const firstRow = response.data[0];
      expect(firstRow.events_type).toBeDefined();
      expect(firstRow.events_timestamp).toBeDefined();
      expect(firstRow["events_properties#total_price"]).toBeDefined();
      expect(firstRow["events_properties#currency"]).toBeDefined();
      expect(firstRow["events_properties#purchase_id"]).toBeDefined();
    });
  });

  describe("count with STRUCT fields", () => {
    it("can count with filter on STRUCT fields", async () => {
      const iql = {
        operation: "count" as const,
        table: "events",
        tableConditions: null,
        whereAndArray: [
          {
            "events.properties#status": {
              equals: "confirmed",
            },
          },
        ],
        operationParameters: {
          count: ["events.type"],
          countDistinct: null,
        },
      };

      const parsed = QuerySchema.parse(iql);
      const response = await count(db, parsed, ctx);
      const { rows: expectedRows } = await sql`
        SELECT COUNT(type) AS event_type_count
        FROM \`events\`
        WHERE properties.status = 'confirmed'
      `.execute(db);
      const expected = expectedRows[0] ?? { event_type_count: 0 };

      expect(response.data).toBeDefined();
      expect(response.data._count).toBeDefined();
      expect(response.data._count["events.type"]).toBe(
        Number(expected.event_type_count),
      );
    });
  });

  describe("groupBy with STRUCT fields", () => {
    it("can group by STRUCT fields", async () => {
      const iql = {
        operation: "groupBy" as const,
        table: "events",
        tableConditions: null,
        whereAndArray: [],
        operationParameters: {
          groupBy: [
            { type: "column", column: "events.properties#payment_method" },
          ],
          count: ["events.type"],
          countDistinct: null,
          sum: null,
          min: null,
          max: null,
          avg: null,
          orderBy: {
            type: "aggregate" as const,
            function: "count",
            column: "events.type",
            direction: "asc" as const,
          },
          limit: 10,
        },
      };

      const parsed = QuerySchema.parse(iql);
      const response = await groupBy(db, parsed, ctx);

      expect(response.data).toBeDefined();
      expect(Array.isArray(response.data)).toBe(true);
      expect(response.data.length).toBeGreaterThan(0);

      // Each group should have payment_method and count
      for (const row of response.data) {
        // Use bracket notation since the key contains a dot
        expect("events.properties#payment_method" in row).toBe(true);
        expect("_count" in row).toBe(true);
      }
    });
  });

  describe("SQL Generation", () => {
    it("generates correct BigQuery SQL for STRUCT field access", async () => {
      // Direct query to verify the STRUCT path syntax works
      const result = await db
        .selectFrom("events")
        .select([
          sql`events.type`.as("type"),
          sql`events.properties.payment_method`.as("payment_method"),
          sql`events.properties.currency`.as("currency"),
        ])
        .limit(5)
        .execute();

      expect(Array.isArray(result)).toBe(true);
      expect(result.length).toBeGreaterThan(0);
      expect(result[0]).toHaveProperty("payment_method");
    });
  });
});
