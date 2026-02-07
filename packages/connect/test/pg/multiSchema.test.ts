// @ts-nocheck
/**
 * Multi-Schema Tests for PostgreSQL
 *
 * Tests schema-qualified table names when querying tables in different schemas.
 * Requires the demo database with both `public` and `hr` schemas.
 */
import { sql } from "kysely";
import type { Kysely } from "kysely";
import { loadTestEnv, getTestContext } from "../loadTestEnv";
import type { SchemaResponse } from "../../src/types/types";
import type { OperationContext } from "../../src/operations/types";

describe("PostgreSQL Multi-Schema Operations", () => {
  let db: Kysely<any>;
  let QuerySchema: (typeof import("~/types/querySchema"))["QuerySchema"];
  let findMany: (typeof import("~/operations/findMany"))["findMany"];
  let count: (typeof import("~/operations/count"))["count"];
  let countRelations: (typeof import("~/operations/countRelations"))["countRelations"];
  let groupBy: (typeof import("~/operations/groupBy"))["groupBy"];
  let aggregate: (typeof import("~/operations/aggregate"))["aggregate"];
  let clearSchemaCache: (typeof import("~/util/schemaCache"))["clearSchemaCache"];

  // Custom context with HR schema tables
  let hrCtx: OperationContext;
  let publicCtx: OperationContext;

  // Helper to check if SQL contains schema-qualified table reference
  // PostgreSQL quotes identifiers, so "hr"."employees" is valid
  const containsSchemaTable = (sqlStr: string, schema: string, table: string) => {
    return (
      sqlStr.includes(`${schema}.${table}`) ||
      sqlStr.includes(`"${schema}"."${table}"`) ||
      sqlStr.includes(`"${schema}".${table}`) ||
      sqlStr.includes(`${schema}."${table}"`)
    );
  };

  beforeAll(async () => {
    loadTestEnv("postgresql");

    jest.resetModules();
    delete (globalThis as any).__INCONVO_KYSELY_DB__;

    ({ clearSchemaCache } = await import("~/util/schemaCache"));
    await clearSchemaCache();

    QuerySchema = (await import("~/types/querySchema")).QuerySchema;
    findMany = (await import("~/operations/findMany")).findMany;
    count = (await import("~/operations/count")).count;
    countRelations = (await import("~/operations/countRelations")).countRelations;
    groupBy = (await import("~/operations/groupBy")).groupBy;
    aggregate = (await import("~/operations/aggregate")).aggregate;
    const { getDb } = await import("~/dbConnection");
    db = await getDb();

    // Build HR schema context manually
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
            {
              name: "projects",
              isList: true,
              targetTable: "projects",
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
            {
              name: "manager",
              isList: false,
              targetTable: "employees",
              targetSchema: "hr",
              sourceColumns: ["manager_id"],
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
      dialect: "postgresql",
    };

    // Get public schema context using existing test helper
    publicCtx = await getTestContext();
  });

  afterAll(async () => {
    if (db) await db.destroy();
    await clearSchemaCache?.();
  });

  describe("findMany with tableSchema", () => {
    it("queries hr.departments with schema qualification", async () => {
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

      // Verify the SQL includes schema qualification (with or without quotes)
      expect(containsSchemaTable(response.query.sql, "hr", "departments")).toBe(true);
      expect(response.data).toBeDefined();
      expect(Array.isArray(response.data)).toBe(true);
      expect(response.data.length).toBeGreaterThan(0);
    });

    it("queries hr.employees with schema qualification", async () => {
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
      // All returned employees should have salary >= 100000
      response.data.forEach((row: any) => {
        expect(Number(row.employees_salary)).toBeGreaterThanOrEqual(100000);
      });
    });

    it("queries hr.projects with status filter", async () => {
      const iql = {
        operation: "findMany" as const,
        table: "projects",
        tableSchema: "hr",
        tableConditions: null,
        whereAndArray: [
          {
            "projects.status": { equals: "active" },
          },
        ],
        operationParameters: {
          select: {
            projects: ["id", "name", "status", "budget"],
          },
          orderBy: { column: "id", direction: "asc" as const },
          limit: 10,
        },
      };

      const parsed = QuerySchema.parse(iql);
      const response = await findMany(db, parsed, hrCtx);

      expect(containsSchemaTable(response.query.sql, "hr", "projects")).toBe(true);
      expect(response.data).toBeDefined();
      response.data.forEach((row: any) => {
        expect(row.projects_status).toBe("active");
      });
    });
  });

  describe("count with tableSchema", () => {
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
        SELECT COUNT(*)::int AS department_count
        FROM "hr"."departments"
      `.execute(db);
      const expected = expectedRows[0] ?? { department_count: 0 };

      expect(containsSchemaTable(response.query.sql, "hr", "departments")).toBe(true);
      expect(response.data._count["departments.id"]).toBe(
        Number(expected.department_count),
      );
    });

    it("counts hr.projects by status", async () => {
      const iql = {
        operation: "count" as const,
        table: "projects",
        tableSchema: "hr",
        tableConditions: null,
        whereAndArray: [
          {
            "projects.status": { equals: "active" },
          },
        ],
        operationParameters: {
          count: ["projects.id"],
        },
      };

      const parsed = QuerySchema.parse(iql);
      const response = await count(db, parsed, hrCtx);
      const { rows: expectedRows } = await sql`
        SELECT COUNT(*)::int AS project_count
        FROM "hr"."projects"
        WHERE "status" = 'active'
      `.execute(db);
      const expected = expectedRows[0] ?? { project_count: 0 };

      expect(containsSchemaTable(response.query.sql, "hr", "projects")).toBe(true);
      expect(response.data._count["projects.id"]).toBe(
        Number(expected.project_count),
      );
    });
  });

  describe("countRelations with tableSchema", () => {
    it("counts hr.employees per hr.department", async () => {
      const iql = {
        operation: "countRelations" as const,
        table: "departments",
        tableSchema: "hr",
        tableConditions: null,
        whereAndArray: [],
        operationParameters: {
          columns: ["id", "name"],
          joins: [
            {
              table: "employees",
              name: "employees",
              path: [
                {
                  source: ["departments.id"],
                  target: ["employees.department_id"],
                },
              ],
            },
          ],
          relationsToCount: [
            {
              name: "employees",
              distinct: null,
            },
          ],
          orderBy: {
            name: "employees",
            direction: "desc" as const,
          },
          limit: 10,
        },
      };

      const parsed = QuerySchema.parse(iql);
      const response = await countRelations(db, parsed, hrCtx);
      const rows = Array.isArray(response) ? response : response.data;

      expect(containsSchemaTable(response.query.sql, "hr", "departments")).toBe(true);
      expect(containsSchemaTable(response.query.sql, "hr", "employees")).toBe(true);
      expect(Array.isArray(rows)).toBe(true);
      expect(rows.length).toBeGreaterThan(0);
      rows.forEach((row: any) => {
        expect(Number.isFinite(Number(row.employees_count))).toBe(true);
      });
    });
  });

  describe("groupBy with tableSchema", () => {
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

    it("groups hr.projects by status with aggregates", async () => {
      const iql = {
        operation: "groupBy" as const,
        table: "projects",
        tableSchema: "hr",
        tableConditions: null,
        whereAndArray: [],
        operationParameters: {
          groupBy: [{ type: "column" as const, column: "projects.status" }],
          count: ["projects.id"],
          countDistinct: null,
          sum: ["projects.budget"],
          min: null,
          max: null,
          avg: null,
          orderBy: { type: "aggregate" as const, function: "count" as const, column: "projects.id", direction: "desc" as const },
          limit: 10,
        },
      };

      const parsed = QuerySchema.parse(iql);
      const response = await groupBy(db, parsed, hrCtx);

      expect(containsSchemaTable(response.query.sql, "hr", "projects")).toBe(true);
      expect(response.data).toBeDefined();
      expect(Array.isArray(response.data)).toBe(true);
    });
  });

  describe("aggregate with tableSchema", () => {
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
      expect(response.data._min).toBeDefined();
      expect(response.data._max).toBeDefined();
      expect(response.data._count).toBeDefined();
    });

    it("aggregates hr.projects budgets", async () => {
      const iql = {
        operation: "aggregate" as const,
        table: "projects",
        tableSchema: "hr",
        tableConditions: null,
        whereAndArray: [],
        operationParameters: {
          sum: ["projects.budget"],
          avg: null,
          min: null,
          max: null,
          count: ["projects.id"],
          countDistinct: null,
          median: null,
        },
      };

      const parsed = QuerySchema.parse(iql);
      const response = await aggregate(db, parsed, hrCtx);

      expect(containsSchemaTable(response.query.sql, "hr", "projects")).toBe(true);
      expect(response.data._sum["projects.budget"]).toBeGreaterThan(0);
    });
  });

  describe("relation filters with tableSchema", () => {
    it("filters hr.employees with department relation filter", async () => {
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
          select: {
            employees: ["id", "first_name", "last_name", "job_title"],
          },
          orderBy: { column: "id", direction: "asc" as const },
          limit: 10,
        },
      };

      const parsed = QuerySchema.parse(iql);
      const response = await findMany(db, parsed, hrCtx);

      // Should use schema-qualified names for both tables in the relation filter
      expect(containsSchemaTable(response.query.sql, "hr", "employees")).toBe(true);
      expect(containsSchemaTable(response.query.sql, "hr", "departments")).toBe(true);
      expect(response.data).toBeDefined();
    });

    it("filters hr.departments with employees some filter", async () => {
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
                    salary: { gte: 150000 },
                  },
                },
              },
            ],
          },
        ],
        operationParameters: {
          select: {
            departments: ["id", "name"],
          },
          orderBy: { column: "id", direction: "asc" as const },
          limit: 10,
        },
      };

      const parsed = QuerySchema.parse(iql);
      const response = await findMany(db, parsed, hrCtx);

      expect(containsSchemaTable(response.query.sql, "hr", "departments")).toBe(true);
      expect(containsSchemaTable(response.query.sql, "hr", "employees")).toBe(true);
      expect(response.data).toBeDefined();
    });
  });

  describe("JOINs with tableSchema", () => {
    it("joins hr.employees with hr.departments", async () => {
      const iql = {
        operation: "findMany" as const,
        table: "employees",
        tableSchema: "hr",
        tableConditions: null,
        whereAndArray: [],
        operationParameters: {
          select: {
            employees: ["id", "first_name", "last_name"],
            "employees.department": ["name"],
          },
          joins: [
            {
              table: "departments",
              name: "employees.department",
              path: [
                {
                  source: ["employees.department_id"],
                  target: ["departments.id"],
                },
              ],
            },
          ],
          orderBy: { column: "id", direction: "asc" as const },
          limit: 10,
        },
      };

      const parsed = QuerySchema.parse(iql);
      const response = await findMany(db, parsed, hrCtx);

      // Should use schema-qualified names for both tables in the JOIN
      expect(containsSchemaTable(response.query.sql, "hr", "employees")).toBe(true);
      expect(containsSchemaTable(response.query.sql, "hr", "departments")).toBe(true);
      expect(response.data).toBeDefined();
      expect(response.data.length).toBeGreaterThan(0);
    });

    it("joins hr.project_assignments with hr.employees and hr.projects", async () => {
      const iql = {
        operation: "findMany" as const,
        table: "project_assignments",
        tableSchema: "hr",
        tableConditions: null,
        whereAndArray: [],
        operationParameters: {
          select: {
            project_assignments: ["id", "role", "hours_allocated"],
            "project_assignments.employee": ["first_name", "last_name"],
            "project_assignments.project": ["name"],
          },
          joins: [
            {
              table: "employees",
              name: "project_assignments.employee",
              path: [
                {
                  source: ["project_assignments.employee_id"],
                  target: ["employees.id"],
                },
              ],
            },
            {
              table: "projects",
              name: "project_assignments.project",
              path: [
                {
                  source: ["project_assignments.project_id"],
                  target: ["projects.id"],
                },
              ],
            },
          ],
          orderBy: { column: "id", direction: "asc" as const },
          limit: 10,
        },
      };

      const parsed = QuerySchema.parse(iql);
      const response = await findMany(db, parsed, hrCtx);

      expect(containsSchemaTable(response.query.sql, "hr", "project_assignments")).toBe(true);
      expect(containsSchemaTable(response.query.sql, "hr", "employees")).toBe(true);
      expect(containsSchemaTable(response.query.sql, "hr", "projects")).toBe(true);
      expect(response.data).toBeDefined();
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
      manualCtx = { schema: manualSchema, dialect: "postgresql" };
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

  describe("verify schema isolation", () => {
    it("public.users and hr.employees are separate tables", async () => {
      // Query public schema users
      const publicQuery = {
        operation: "count" as const,
        table: "users",
        tableConditions: null,
        whereAndArray: [],
        operationParameters: {
          count: ["users.id"],
        },
      };

      // Query hr schema employees
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

      const publicParsed = QuerySchema.parse(publicQuery);
      const hrParsed = QuerySchema.parse(hrQuery);

      const publicResponse = await count(db, publicParsed, publicCtx);
      const hrResponse = await count(db, hrParsed, hrCtx);
      const { rows: expectedPublicRows } = await sql`
        SELECT COUNT(*)::int AS user_count
        FROM "public"."users"
      `.execute(db);
      const expectedPublic = expectedPublicRows[0] ?? { user_count: 0 };
      const { rows: expectedHrRows } = await sql`
        SELECT COUNT(*)::int AS employee_count
        FROM "hr"."employees"
      `.execute(db);
      const expectedHr = expectedHrRows[0] ?? { employee_count: 0 };

      // Public should NOT contain hr schema qualification
      expect(containsSchemaTable(publicResponse.query.sql, "hr", "users")).toBe(false);
      // HR should contain hr schema qualification
      expect(containsSchemaTable(hrResponse.query.sql, "hr", "employees")).toBe(true);

      // Both should have data
      expect(publicResponse.data._count["users.id"]).toBe(
        Number(expectedPublic.user_count),
      );
      expect(hrResponse.data._count["employees.id"]).toBe(
        Number(expectedHr.employee_count),
      );
    });
  });
});
