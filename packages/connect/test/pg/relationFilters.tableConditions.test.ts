// @ts-nocheck
import { sql, type Kysely } from "kysely";
import { loadTestEnv, getTestContext } from "../loadTestEnv";

/**
 * Tests for relation filters (some, every, none) with tableConditions.
 *
 * tableConditions is used to apply row-level security (e.g., tenant filtering)
 * to relation subqueries. When a target table has a condition configured,
 * the condition should be included in the EXISTS/NOT EXISTS subqueries.
 */
describe("PostgreSQL Relation Filters with tableConditions", () => {
  let db: Kysely<any>;
  let QuerySchema: (typeof import("~/types/querySchema"))["QuerySchema"];
  let findMany: (typeof import("~/operations/findMany"))["findMany"];
  let count: (typeof import("~/operations/count"))["count"];
  let getCachedSchema: (typeof import("~/util/schemaCache"))["getCachedSchema"];
  let ctx: Awaited<ReturnType<typeof getTestContext>>;

  beforeAll(async () => {
    loadTestEnv("postgresql");

    jest.resetModules();
    delete (globalThis as any).__INCONVO_KYSELY_DB__;

    QuerySchema = (await import("~/types/querySchema")).QuerySchema;
    findMany = (await import("~/operations/findMany")).findMany;
    count = (await import("~/operations/count")).count;
    getCachedSchema = (await import("~/util/schemaCache")).getCachedSchema;
    const { getDb } = await import("~/dbConnection");
    db = await getDb();
    ctx = await getTestContext();
  });

  afterAll(async () => {
    if (db) await db.destroy();
  });

  describe("'some' operator with tableConditions", () => {
    test("filters users who have at least one order with table condition applied", async () => {
      // First, get the schema to check available relations
      const schema = await getCachedSchema();
      const usersTable = schema.tables.find((t) => t.name === "users");

      // Find a relation from users to orders (if it exists)
      const ordersRelation = usersTable?.relations?.find(
        (r) => r.targetTable === "orders",
      );

      if (!ordersRelation) {
        // Skip if relation doesn't exist in schema
        console.log(
          "Skipping test: users -> orders relation not found in schema",
        );
        return;
      }

      // Get a sample user_id to use as the table condition value
      const sampleOrder = await db
        .selectFrom("orders")
        .select("user_id")
        .limit(1)
        .executeTakeFirst();

      if (!sampleOrder) {
        console.log("Skipping test: no orders in database");
        return;
      }

      const iql = {
        operation: "findMany" as const,
        table: "users",
        whereAndArray: [
          {
            AND: [
              {
                [ordersRelation.name]: {
                  some: {},
                },
              },
            ],
          },
        ],
        // Apply a tableCondition on orders - this simulates row-level security
        // where only orders matching user_id = X should be considered
        tableConditions: {
          orders: {
            column: "user_id",
            value: sampleOrder.user_id,
          },
        },
        operationParameters: {
          select: {
            users: ["id", "name"],
          },
          orderBy: null,
          limit: 10,
        },
      };

      const parsed = QuerySchema.parse(iql);
      const response = await findMany(db, parsed, ctx);

      // Verify that we got results
      expect(response.data).toBeDefined();
      expect(Array.isArray(response.data)).toBe(true);

      // Manually execute the equivalent query to verify
      // The subquery should include the tableCondition
      const sourceCol = ordersRelation.sourceColumns?.[0] || "id";
      const targetCol = ordersRelation.targetColumns?.[0] || "user_id";

      const expectedRows = await db
        .selectFrom("users")
        .select(["id", "name"])
        .where(
          sql`EXISTS (SELECT 1 FROM orders WHERE orders.${sql.ref(targetCol)} = users.${sql.ref(sourceCol)} AND orders.user_id = ${sampleOrder.user_id})`,
        )
        .limit(10)
        .execute();

      // Both queries should return the same users
      expect(response.data?.length).toBe(expectedRows.length);
    });

    test("applies table condition with nested filter conditions", async () => {
      const schema = await getCachedSchema();
      const usersTable = schema.tables.find((t) => t.name === "users");
      const ordersRelation = usersTable?.relations?.find(
        (r) => r.targetTable === "orders",
      );

      if (!ordersRelation) {
        console.log(
          "Skipping test: users -> orders relation not found in schema",
        );
        return;
      }

      // Get a sample user_id and min subtotal
      const sampleOrder = await db
        .selectFrom("orders")
        .select(["user_id", "subtotal"])
        .where("subtotal", ">", 0)
        .limit(1)
        .executeTakeFirst();

      if (!sampleOrder) {
        console.log("Skipping test: no orders with positive subtotal");
        return;
      }

      const iql = {
        operation: "findMany" as const,
        table: "users",
        whereAndArray: [
          {
            AND: [
              {
                [ordersRelation.name]: {
                  some: {
                    "orders.subtotal": { gt: 0 },
                  },
                },
              },
            ],
          },
        ],
        tableConditions: {
          orders: {
            column: "user_id",
            value: sampleOrder.user_id,
          },
        },
        operationParameters: {
          select: {
            users: ["id", "name"],
          },
          orderBy: null,
          limit: 10,
        },
      };

      const parsed = QuerySchema.parse(iql);
      const response = await findMany(db, parsed, ctx);

      expect(response.data).toBeDefined();
      expect(Array.isArray(response.data)).toBe(true);

      // The query should have applied both the nested condition (subtotal > 0)
      // and the tableCondition (user_id = X) in the EXISTS subquery
    });
  });

  describe("'none' operator with tableConditions", () => {
    test("filters users who have no orders matching table condition", async () => {
      const schema = await getCachedSchema();
      const usersTable = schema.tables.find((t) => t.name === "users");
      const ordersRelation = usersTable?.relations?.find(
        (r) => r.targetTable === "orders",
      );

      if (!ordersRelation) {
        console.log(
          "Skipping test: users -> orders relation not found in schema",
        );
        return;
      }

      // Use a non-existent user_id to guarantee "none" returns results
      const iql = {
        operation: "findMany" as const,
        table: "users",
        whereAndArray: [
          {
            AND: [
              {
                [ordersRelation.name]: {
                  none: {},
                },
              },
            ],
          },
        ],
        // With this table condition, only orders with user_id = -9999 are considered
        // Since no such orders exist, "none" should return all users
        tableConditions: {
          orders: {
            column: "user_id",
            value: -9999,
          },
        },
        operationParameters: {
          select: {
            users: ["id", "name"],
          },
          orderBy: null,
          limit: 100,
        },
      };

      const parsed = QuerySchema.parse(iql);
      const response = await findMany(db, parsed, ctx);

      expect(response.data).toBeDefined();
      expect(Array.isArray(response.data)).toBe(true);

      // Get total user count
      const totalUsers = await db
        .selectFrom("users")
        .select(sql<number>`COUNT(*)::int`.as("count"))
        .executeTakeFirst();

      // Since tableCondition filters to non-existent orders,
      // "none" should return all users (up to limit)
      const expectedCount = Math.min(totalUsers?.count || 0, 100);
      expect(response.data?.length).toBe(expectedCount);
    });
  });

  describe("'every' operator with tableConditions", () => {
    test("filters users where all matching orders satisfy condition", async () => {
      const schema = await getCachedSchema();
      const usersTable = schema.tables.find((t) => t.name === "users");
      const ordersRelation = usersTable?.relations?.find(
        (r) => r.targetTable === "orders",
      );

      if (!ordersRelation) {
        console.log(
          "Skipping test: users -> orders relation not found in schema",
        );
        return;
      }

      // Get a user who has orders
      const userWithOrders = await db
        .selectFrom("users as u")
        .select("u.id")
        .innerJoin("orders as o", "o.user_id", "u.id")
        .limit(1)
        .executeTakeFirst();

      if (!userWithOrders) {
        console.log("Skipping test: no users with orders found");
        return;
      }

      // Get the min subtotal for this user's orders
      const minSubtotal = await db
        .selectFrom("orders")
        .select(sql<number>`MIN(subtotal)`.as("min"))
        .where("user_id", "=", userWithOrders.id)
        .executeTakeFirst();

      const iql = {
        operation: "findMany" as const,
        table: "users",
        whereAndArray: [
          {
            AND: [
              {
                [ordersRelation.name]: {
                  every: {
                    "orders.subtotal": { gte: minSubtotal?.min || 0 },
                  },
                },
              },
            ],
          },
        ],
        tableConditions: {
          orders: {
            column: "user_id",
            value: userWithOrders.id,
          },
        },
        operationParameters: {
          select: {
            users: ["id", "name"],
          },
          orderBy: null,
          limit: 10,
        },
      };

      const parsed = QuerySchema.parse(iql);
      const response = await findMany(db, parsed, ctx);

      expect(response.data).toBeDefined();
      expect(Array.isArray(response.data)).toBe(true);
    });
  });

  describe("count operation with relation filters and tableConditions", () => {
    test("counts users with orders filtered by tableConditions", async () => {
      const schema = await getCachedSchema();
      const usersTable = schema.tables.find((t) => t.name === "users");
      const ordersRelation = usersTable?.relations?.find(
        (r) => r.targetTable === "orders",
      );

      if (!ordersRelation) {
        console.log(
          "Skipping test: users -> orders relation not found in schema",
        );
        return;
      }

      // Get a sample user_id
      const sampleOrder = await db
        .selectFrom("orders")
        .select("user_id")
        .limit(1)
        .executeTakeFirst();

      if (!sampleOrder) {
        console.log("Skipping test: no orders in database");
        return;
      }

      const iql = {
        operation: "count" as const,
        table: "users",
        whereAndArray: [
          {
            AND: [
              {
                [ordersRelation.name]: {
                  some: {},
                },
              },
            ],
          },
        ],
        tableConditions: {
          orders: {
            column: "user_id",
            value: sampleOrder.user_id,
          },
        },
        operationParameters: {
          count: ["users.id"],
          countDistinct: null,
        },
      };

      const parsed = QuerySchema.parse(iql);
      const response = await count(db, parsed, ctx);

      expect(response.data).toBeDefined();
      expect(response.data._count).toBeDefined();
      expect(typeof response.data._count["users.id"]).toBe("number");
    });
  });

  describe("tableConditions on multiple tables", () => {
    test("applies conditions to nested relation filters", async () => {
      const schema = await getCachedSchema();
      const usersTable = schema.tables.find((t) => t.name === "users");
      const ordersRelation = usersTable?.relations?.find(
        (r) => r.targetTable === "orders",
      );

      if (!ordersRelation) {
        console.log(
          "Skipping test: users -> orders relation not found in schema",
        );
        return;
      }

      // Get sample values
      const sampleUser = await db
        .selectFrom("users")
        .select("id")
        .limit(1)
        .executeTakeFirst();

      const sampleOrder = await db
        .selectFrom("orders")
        .select("user_id")
        .limit(1)
        .executeTakeFirst();

      if (!sampleUser || !sampleOrder) {
        console.log("Skipping test: no users or orders in database");
        return;
      }

      // Query with tableConditions on both users and orders
      const iql = {
        operation: "findMany" as const,
        table: "users",
        whereAndArray: [
          {
            AND: [
              {
                [ordersRelation.name]: {
                  some: {},
                },
              },
            ],
          },
        ],
        tableConditions: {
          users: {
            column: "id",
            value: sampleUser.id,
          },
          orders: {
            column: "user_id",
            value: sampleOrder.user_id,
          },
        },
        operationParameters: {
          select: {
            users: ["id", "name"],
          },
          orderBy: null,
          limit: 10,
        },
      };

      const parsed = QuerySchema.parse(iql);
      const response = await findMany(db, parsed, ctx);

      // Should succeed without errors
      expect(response.data).toBeDefined();
      expect(Array.isArray(response.data)).toBe(true);
    });
  });

  describe("null/undefined tableConditions", () => {
    test("works correctly when tableConditions is null", async () => {
      const schema = await getCachedSchema();
      const usersTable = schema.tables.find((t) => t.name === "users");
      const ordersRelation = usersTable?.relations?.find(
        (r) => r.targetTable === "orders",
      );

      if (!ordersRelation) {
        console.log(
          "Skipping test: users -> orders relation not found in schema",
        );
        return;
      }

      const iql = {
        operation: "findMany" as const,
        table: "users",
        whereAndArray: [
          {
            AND: [
              {
                [ordersRelation.name]: {
                  some: {},
                },
              },
            ],
          },
        ],
        tableConditions: null,
        operationParameters: {
          select: {
            users: ["id", "name"],
          },
          orderBy: null,
          limit: 10,
        },
      };

      const parsed = QuerySchema.parse(iql);
      const response = await findMany(db, parsed, ctx);

      expect(response.data).toBeDefined();
      expect(Array.isArray(response.data)).toBe(true);
    });

    test("rejects query when tableConditions is omitted (required field)", async () => {
      const iql = {
        operation: "findMany" as const,
        table: "users",
        whereAndArray: [],
        // tableConditions omitted - should fail validation
        operationParameters: {
          select: {
            users: ["id", "name"],
          },
          orderBy: null,
          limit: 10,
        },
      };

      expect(() => QuerySchema.parse(iql)).toThrow();
    });
  });
});
