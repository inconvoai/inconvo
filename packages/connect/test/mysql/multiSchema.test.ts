// @ts-nocheck
/**
 * Multi-Schema Tests for MySQL
 *
 * MySQL uses separate databases instead of schemas.
 * Tests schema-qualified table names when querying tables in different databases.
 * Requires the demo database with both `hosted` and `hr` databases.
 */
import { sql } from "kysely";
import type { Kysely } from "kysely";
import { loadTestEnv, getTestContext } from "../loadTestEnv";
import type { SchemaResponse } from "../../src/types/types";
import type { OperationContext } from "../../src/operations/types";

describe("MySQL Multi-Schema Operations", () => {
  let db: Kysely<any>;
  let QuerySchema: (typeof import("~/types/querySchema"))["QuerySchema"];
  let findMany: (typeof import("~/operations/findMany"))["findMany"];
  let count: (typeof import("~/operations/count"))["count"];
  let groupBy: (typeof import("~/operations/groupBy"))["groupBy"];
  let aggregate: (typeof import("~/operations/aggregate"))["aggregate"];
  let clearSchemaCache: (typeof import("~/util/schemaCache"))["clearSchemaCache"];

  // Custom context with HR database tables
  let hrCtx: OperationContext;
  let publicCtx: OperationContext;

  // Helper to check if SQL contains schema-qualified table reference
  // MySQL uses backticks for identifiers, so `hr`.`employees` is valid
  const containsSchemaTable = (sqlStr: string, schema: string, table: string) => {
    return (
      sqlStr.includes(`${schema}.${table}`) ||
      sqlStr.includes(`\`${schema}\`.\`${table}\``) ||
      sqlStr.includes(`\`${schema}\`.${table}`) ||
      sqlStr.includes(`${schema}.\`${table}\``)
    );
  };

  beforeAll(async () => {
    loadTestEnv("mysql");

    jest.resetModules();
    delete (globalThis as any).__INCONVO_KYSELY_DB__;

    ({ clearSchemaCache } = await import("~/util/schemaCache"));
    await clearSchemaCache();

    QuerySchema = (await import("~/types/querySchema")).QuerySchema;
    findMany = (await import("~/operations/findMany")).findMany;
    count = (await import("~/operations/count")).count;
    groupBy = (await import("~/operations/groupBy")).groupBy;
    aggregate = (await import("~/operations/aggregate")).aggregate;
    const { getDb } = await import("~/dbConnection");
    db = await getDb();

    // Build HR schema context manually (MySQL uses database name as schema)
    const hrSchema: SchemaResponse = {
      tables: [
        {
          name: "departments",
          schema: "hr",
          columns: [
            { name: "id", type: "number" },
            { name: "name", type: "string" },
            { name: "budget", type: "number" },
            { name: "location", type: "string" },
            { name: "created_at", type: "DateTime" },
          ],
          relations: [
            {
              name: "employees",
              isList: true,
              targetTable: "employees",
              targetSchema: "hr",
              sourceColumns: ["id"],
              targetColumns: ["department_id"],
            },
          ],
        },
        {
          name: "employees",
          schema: "hr",
          columns: [
            { name: "id", type: "number" },
            { name: "department_id", type: "number" },
            { name: "first_name", type: "string" },
            { name: "last_name", type: "string" },
            { name: "email", type: "string" },
            { name: "hire_date", type: "DateTime" },
            { name: "salary", type: "number" },
            { name: "job_title", type: "string" },
            { name: "manager_id", type: "number" },
            { name: "is_active", type: "boolean" },
          ],
          relations: [
            {
              name: "department",
              isList: false,
              targetTable: "departments",
              targetSchema: "hr",
              sourceColumns: ["department_id"],
              targetColumns: ["id"],
            },
          ],
        },
        {
          name: "projects",
          schema: "hr",
          columns: [
            { name: "id", type: "number" },
            { name: "name", type: "string" },
            { name: "department_id", type: "number" },
            { name: "start_date", type: "DateTime" },
            { name: "end_date", type: "DateTime" },
            { name: "status", type: "string" },
            { name: "budget", type: "number" },
          ],
          relations: [],
        },
        {
          name: "project_assignments",
          schema: "hr",
          columns: [
            { name: "id", type: "number" },
            { name: "project_id", type: "number" },
            { name: "employee_id", type: "number" },
            { name: "role", type: "string" },
            { name: "hours_allocated", type: "number" },
            { name: "assigned_at", type: "DateTime" },
          ],
          relations: [],
        },
        {
          name: "time_entries",
          schema: "hr",
          columns: [
            { name: "id", type: "number" },
            { name: "employee_id", type: "number" },
            { name: "project_id", type: "number" },
            { name: "date", type: "DateTime" },
            { name: "hours", type: "number" },
            { name: "description", type: "string" },
          ],
          relations: [],
        },
      ],
      databaseSchemas: ["hr"],
    };

    hrCtx = {
      schema: hrSchema,
      dialect: "mysql",
    };

    // Get public schema context using existing test helper
    publicCtx = await getTestContext();
  });

  afterAll(async () => {
    if (db) await db.destroy();
    await clearSchemaCache?.();
  });

  describe("findMany with tableSchema (database)", () => {
    it("queries hr.departments with database qualification", async () => {
      const iql = {
        operation: "findMany" as const,
        table: "departments",
        tableSchema: "hr",
        tableConditions: null,
        whereAndArray: [],
        operationParameters: {
          select: {
            departments: ["id", "name", "budget", "location"],
          },
          orderBy: { column: "id", direction: "asc" as const },
          limit: 10,
        },
      };

      const parsed = QuerySchema.parse(iql);
      const response = await findMany(db, parsed, hrCtx);

      // Verify the SQL includes database qualification
      expect(containsSchemaTable(response.query.sql, "hr", "departments")).toBe(true);
      expect(response.data).toBeDefined();
      expect(Array.isArray(response.data)).toBe(true);
      expect(response.data.length).toBeGreaterThan(0);
    });

    it("queries hr.employees with database qualification", async () => {
      const iql = {
        operation: "findMany" as const,
        table: "employees",
        tableSchema: "hr",
        tableConditions: null,
        whereAndArray: [],
        operationParameters: {
          select: {
            employees: ["id", "first_name", "last_name", "salary", "job_title"],
          },
          orderBy: { column: "id", direction: "asc" as const },
          limit: 10,
        },
      };

      const parsed = QuerySchema.parse(iql);
      const response = await findMany(db, parsed, hrCtx);

      expect(containsSchemaTable(response.query.sql, "hr", "employees")).toBe(true);
      expect(response.data).toBeDefined();
      expect(response.data.length).toBeGreaterThan(0);
    });

    it("queries hr.employees with WHERE conditions", async () => {
      const iql = {
        operation: "findMany" as const,
        table: "employees",
        tableSchema: "hr",
        tableConditions: null,
        whereAndArray: [
          {
            "employees.salary": { gte: 100000 },
          },
        ],
        operationParameters: {
          select: {
            employees: ["id", "first_name", "last_name", "salary"],
          },
          orderBy: { column: "salary", direction: "desc" as const },
          limit: 5,
        },
      };

      const parsed = QuerySchema.parse(iql);
      const response = await findMany(db, parsed, hrCtx);

      expect(containsSchemaTable(response.query.sql, "hr", "employees")).toBe(true);
      expect(response.data).toBeDefined();
    });
  });

  describe("count with tableSchema (database)", () => {
    it("counts hr.employees", async () => {
      const iql = {
        operation: "count" as const,
        table: "employees",
        tableSchema: "hr",
        tableConditions: null,
        whereAndArray: [],
        operationParameters: {
          count: ["employees.id"],
        },
      };

      const parsed = QuerySchema.parse(iql);
      const response = await count(db, parsed, hrCtx);

      expect(containsSchemaTable(response.query.sql, "hr", "employees")).toBe(true);
      expect(response.data).toBeDefined();
      expect(response.data._count).toBeDefined();
    });

    it("counts hr.departments", async () => {
      const iql = {
        operation: "count" as const,
        table: "departments",
        tableSchema: "hr",
        tableConditions: null,
        whereAndArray: [],
        operationParameters: {
          count: ["departments.id"],
        },
      };

      const parsed = QuerySchema.parse(iql);
      const response = await count(db, parsed, hrCtx);
      const { rows: expectedRows } = await sql`
        SELECT COUNT(*) AS department_count
        FROM \`hr\`.\`departments\`
      `.execute(db);
      const expected = expectedRows[0] ?? { department_count: 0 };

      expect(containsSchemaTable(response.query.sql, "hr", "departments")).toBe(true);
      expect(response.data._count["departments.id"]).toBe(
        Number(expected.department_count),
      );
    });
  });

  describe("groupBy with tableSchema (database)", () => {
    it("groups hr.employees by department", async () => {
      const iql = {
        operation: "groupBy" as const,
        table: "employees",
        tableSchema: "hr",
        tableConditions: null,
        whereAndArray: [],
        operationParameters: {
          groupBy: [{ type: "column" as const, column: "employees.department_id" }],
          count: ["employees.id"],
          countDistinct: null,
          sum: null,
          min: null,
          max: null,
          avg: null,
          orderBy: { type: "aggregate" as const, function: "count" as const, column: "employees.id", direction: "desc" as const },
          limit: 10,
        },
      };

      const parsed = QuerySchema.parse(iql);
      const response = await groupBy(db, parsed, hrCtx);

      expect(containsSchemaTable(response.query.sql, "hr", "employees")).toBe(true);
      expect(response.data).toBeDefined();
      expect(Array.isArray(response.data)).toBe(true);
    });
  });

  describe("aggregate with tableSchema (database)", () => {
    it("aggregates hr.employees salaries", async () => {
      const iql = {
        operation: "aggregate" as const,
        table: "employees",
        tableSchema: "hr",
        tableConditions: null,
        whereAndArray: [],
        operationParameters: {
          sum: ["employees.salary"],
          avg: ["employees.salary"],
          min: ["employees.salary"],
          max: ["employees.salary"],
          count: ["employees.id"],
          countDistinct: null,
          median: null,
        },
      };

      const parsed = QuerySchema.parse(iql);
      const response = await aggregate(db, parsed, hrCtx);

      expect(containsSchemaTable(response.query.sql, "hr", "employees")).toBe(true);
      expect(response.data).toBeDefined();
      expect(response.data._sum).toBeDefined();
      expect(response.data._avg).toBeDefined();
    });
  });

  describe("manual relation filters with tableSchema", () => {
    // Simulates manual relations where targetSchema is NOT explicitly set.
    // The query builder should fall back to the target table's schema property.
    let manualCtx: OperationContext;

    beforeAll(() => {
      const manualSchema: SchemaResponse = {
        tables: [
          {
            name: "departments",
            schema: "hr",
            columns: [
              { name: "id", type: "number" },
              { name: "name", type: "string" },
              { name: "budget", type: "number" },
              { name: "location", type: "string" },
            ],
            relations: [
              {
                name: "employees",
                isList: true,
                targetTable: "employees",
                // targetSchema intentionally omitted — tests the fallback
                sourceColumns: ["id"],
                targetColumns: ["department_id"],
                source: "MANUAL",
              },
            ],
          },
          {
            name: "employees",
            schema: "hr",
            columns: [
              { name: "id", type: "number" },
              { name: "department_id", type: "number" },
              { name: "first_name", type: "string" },
              { name: "last_name", type: "string" },
              { name: "salary", type: "number" },
            ],
            relations: [
              {
                name: "department",
                isList: false,
                targetTable: "departments",
                // targetSchema intentionally omitted — tests the fallback
                sourceColumns: ["department_id"],
                targetColumns: ["id"],
                source: "MANUAL",
              },
            ],
          },
        ],
        databaseSchemas: ["hr"],
      };
      manualCtx = { schema: manualSchema, dialect: "mysql" };
    });

    it("schema-qualifies target table in EXISTS for manual 'is' relation filter", async () => {
      const iql = {
        operation: "findMany" as const,
        table: "employees",
        tableSchema: "hr",
        tableConditions: null,
        whereAndArray: [
          {
            AND: [
              {
                department: {
                  is: {
                    name: { equals: "Engineering" },
                  },
                },
              },
            ],
          },
        ],
        operationParameters: {
          select: { employees: ["id", "first_name", "last_name"] },
          orderBy: { column: "id", direction: "asc" as const },
          limit: 10,
        },
      };

      const parsed = QuerySchema.parse(iql);
      const response = await findMany(db, parsed, manualCtx);

      expect(containsSchemaTable(response.query.sql, "hr", "employees")).toBe(true);
      expect(containsSchemaTable(response.query.sql, "hr", "departments")).toBe(true);
      expect(response.data).toBeDefined();
    });

    it("schema-qualifies target table in EXISTS for manual 'some' relation filter", async () => {
      const iql = {
        operation: "findMany" as const,
        table: "departments",
        tableSchema: "hr",
        tableConditions: null,
        whereAndArray: [
          {
            AND: [
              {
                employees: {
                  some: {
                    salary: { gte: 100000 },
                  },
                },
              },
            ],
          },
        ],
        operationParameters: {
          select: { departments: ["id", "name"] },
          orderBy: { column: "id", direction: "asc" as const },
          limit: 10,
        },
      };

      const parsed = QuerySchema.parse(iql);
      const response = await findMany(db, parsed, manualCtx);

      expect(containsSchemaTable(response.query.sql, "hr", "departments")).toBe(true);
      expect(containsSchemaTable(response.query.sql, "hr", "employees")).toBe(true);
      expect(response.data).toBeDefined();
    });
  });

  describe("verify database isolation", () => {
    it("hosted.users and hr.employees are separate tables", async () => {
      // Query hosted database users
      const hostedQuery = {
        operation: "count" as const,
        table: "users",
        tableConditions: null,
        whereAndArray: [],
        operationParameters: {
          count: ["users.id"],
        },
      };

      // Query hr database employees
      const hrQuery = {
        operation: "count" as const,
        table: "employees",
        tableSchema: "hr",
        tableConditions: null,
        whereAndArray: [],
        operationParameters: {
          count: ["employees.id"],
        },
      };

      const hostedParsed = QuerySchema.parse(hostedQuery);
      const hrParsed = QuerySchema.parse(hrQuery);

      const hostedResponse = await count(db, hostedParsed, publicCtx);
      const hrResponse = await count(db, hrParsed, hrCtx);
      const { rows: expectedHostedRows } = await sql`
        SELECT COUNT(*) AS user_count
        FROM \`users\`
      `.execute(db);
      const expectedHosted = expectedHostedRows[0] ?? { user_count: 0 };
      const { rows: expectedHrRows } = await sql`
        SELECT COUNT(*) AS employee_count
        FROM \`hr\`.\`employees\`
      `.execute(db);
      const expectedHr = expectedHrRows[0] ?? { employee_count: 0 };

      // Hosted should NOT contain hr database qualification
      expect(containsSchemaTable(hostedResponse.query.sql, "hr", "users")).toBe(false);
      // HR should contain hr database qualification
      expect(containsSchemaTable(hrResponse.query.sql, "hr", "employees")).toBe(true);

      // Both should have data
      expect(hostedResponse.data._count["users.id"]).toBe(
        Number(expectedHosted.user_count),
      );
      expect(hrResponse.data._count["employees.id"]).toBe(
        Number(expectedHr.employee_count),
      );
    });
  });
});
