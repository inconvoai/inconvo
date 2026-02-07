// @ts-nocheck
import { sql, type Kysely } from "kysely";
import { loadTestEnv, getTestContext } from "../loadTestEnv";

describe("PostgreSQL countRelations Operation", () => {
  let db: Kysely<any>;
  let QuerySchema: (typeof import("~/types/querySchema"))["QuerySchema"];
  let countRelations: (typeof import("~/operations/countRelations"))["countRelations"];
  let ctx: Awaited<ReturnType<typeof getTestContext>>;

  beforeAll(async () => {
    loadTestEnv("postgresql");

    jest.resetModules();
    delete (globalThis as any).__INCONVO_KYSELY_DB__;

    QuerySchema = (await import("~/types/querySchema")).QuerySchema;
    countRelations = (await import("~/operations/countRelations"))
      .countRelations;
    const { getDb } = await import("~/dbConnection");
    db = await getDb();
    ctx = await getTestContext();
  });

  afterAll(async () => {
    if (db) await db.destroy();
  });

  test("How many orders does each product have (order by most orders)?", async () => {
    const iql = {
      table: "products",
      tableConditions: null,
      whereAndArray: [],
      operation: "countRelations" as const,
      operationParameters: {
        columns: ["id", "title"],
        joins: [
          {
            table: "orders",
            name: "orders",
            path: [
              {
                source: ["products.id"],
                target: ["orders.product_id"],
              },
            ],
          },
        ],
        relationsToCount: [
          {
            name: "orders",
            distinct: null,
          },
        ],
        orderBy: {
          name: "orders",
          direction: "desc" as const,
        },
        limit: 1,
      },
    };

    const parsed = QuerySchema.parse(iql);
    const response = await countRelations(db, parsed, ctx);

    const expectedRows = await db
      .selectFrom("products as p")
      .innerJoin("orders as o", "o.product_id", "p.id")
      .select([
        sql`p.id`.as("id"),
        sql`p.title`.as("title"),
        sql<number>`COUNT(*)::int`.as("orders_count"),
      ])
      .groupBy(["p.id", "p.title"])
      .orderBy("orders_count", "desc")
      .orderBy("p.id", "asc")
      .limit(1)
      .execute();

    const expected = expectedRows.map((row: any) => ({
      id: row.id,
      title: row.title,
      orders_count: Number(row.orders_count),
    }));

    const resultRows = Array.isArray(response) ? response : response.data;

    expect(resultRows).toEqual(expected);
  }, 10000);

  test("How many distinct orders has each user placed?", async () => {
    const iql = {
      table: "users",
      tableConditions: null,
      whereAndArray: [],
      operation: "countRelations" as const,
      operationParameters: {
        columns: ["id"],
        joins: [
          {
            table: "orders",
            name: "orders",
            path: [
              {
                source: ["users.id"],
                target: ["orders.user_id"],
              },
            ],
            joinType: "left",
          },
        ],
        relationsToCount: [
          {
            name: "orders",
            distinct: "orders.id",
          },
        ],
        orderBy: null,
        limit: 5,
      },
    };

    const parsed = QuerySchema.parse(iql);
    const response = await countRelations(db, parsed, ctx);

    const expectedRows = await db
      .selectFrom("users as u")
      .leftJoin("orders as o", "o.user_id", "u.id")
      .select([
        sql`u.id`.as("id"),
        sql<number>`COUNT(DISTINCT o.id)::int`.as("orders_distinctCount"),
      ])
      .groupBy("u.id")
      .orderBy("u.id", "asc")
      .limit(5)
      .execute();

    const expected = expectedRows.map((row: any) => ({
      id: row.id,
      orders_distinctCount: Number(row.orders_distinctCount),
    }));

    const resultRows = Array.isArray(response) ? response : response.data;

    expect(resultRows).toEqual(expected);
  }, 10000);

  test("countRelations works when the base table is a SQL virtual table", async () => {
    const usersTable = ctx.schema.tables.find((t: any) => t.name === "users");
    expect(usersTable).toBeTruthy();

    const virtualSchema = {
      ...ctx.schema,
      tables: [
        ...ctx.schema.tables,
        {
          name: "virtual_users",
          columns: [{ name: "id", type: "number" }],
          relations: [
            {
              name: "orders",
              isList: true,
              targetTable: "orders",
              ...(usersTable?.schema ? { targetSchema: usersTable.schema } : {}),
              sourceColumns: ["id"],
              targetColumns: ["user_id"],
            },
          ],
          computedColumns: [],
          columnConversions: [],
          virtualTable: {
            sql: "SELECT id FROM users",
            dialect: "postgresql",
            sourceColumns: [{ sourceName: "id", name: "id" }],
          },
        },
      ],
    };

    const iql = {
      table: "virtual_users",
      tableConditions: null,
      whereAndArray: [],
      operation: "countRelations" as const,
      operationParameters: {
        columns: ["id"],
  test("Returns an empty array when no rows match", async () => {
    const iql = {
      table: "products",
      tableConditions: null,
      whereAndArray: [
        {
          "products.title": {
            equals: "__count_relations_missing_title_9f9a0ff0__",
          },
        },
      ],
      operation: "countRelations" as const,
      operationParameters: {
        columns: ["id", "title"],
        joins: [
          {
            table: "orders",
            name: "orders",
            path: [
              {
                source: ["products.id"],
                target: ["orders.product_id"],
              },
            ],
          },
        ],
        relationsToCount: [
          {
            name: "orders",
            distinct: null,
          },
        ],
        orderBy: null,
        limit: 5,
      },
    };

    const parsed = QuerySchema.parse(iql);
    const response = await countRelations(db, parsed, ctx);
    const resultRows = Array.isArray(response) ? response : response.data;

    expect(Array.isArray(resultRows)).toBe(true);
    expect(resultRows).toEqual([]);
  }, 10000);

  test("countRelations works when the base table is a SQL virtual table", async () => {
    const usersTable = ctx.schema.tables.find((t: any) => t.name === "users");
    expect(usersTable).toBeTruthy();

    const virtualSchema = {
      ...ctx.schema,
      tables: [
        ...ctx.schema.tables,
        {
          name: "virtual_users",
          columns: [{ name: "id", type: "number" }],
          relations: [
            {
              name: "orders",
              isList: true,
              targetTable: "orders",
              ...(usersTable?.schema ? { targetSchema: usersTable.schema } : {}),
              sourceColumns: ["id"],
              targetColumns: ["user_id"],
            },
          ],
          computedColumns: [],
          columnConversions: [],
          virtualTable: {
            sql: "SELECT id FROM users",
            dialect: "postgresql",
            sourceColumns: [{ sourceName: "id", name: "id" }],
          },
        },
      ],
    };

    const iql = {
      table: "virtual_users",
      tableConditions: null,
      whereAndArray: [],
      operation: "countRelations" as const,
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
            joinType: "left",
          },
        ],
        relationsToCount: [
          {
            name: "orders",
            distinct: "orders.id",
          },
        ],
        orderBy: null,
        limit: 5,
      },
    };

    const parsed = QuerySchema.parse(iql);
    const response = await countRelations(db, parsed, {
      ...ctx,
      schema: virtualSchema,
    });

    const expectedRows = await db
      .selectFrom("users as u")
      .leftJoin("orders as o", "o.user_id", "u.id")
      .select([
        sql`u.id`.as("id"),
        sql<number>`COUNT(DISTINCT o.id)::int`.as("orders_distinctCount"),
      ])
      .groupBy("u.id")
      .orderBy("u.id", "asc")
      .limit(5)
      .execute();

    const expected = expectedRows.map((row: any) => ({
      id: row.id,
      orders_distinctCount: Number(row.orders_distinctCount),
    }));

    const resultRows = Array.isArray(response) ? response : response.data;

    expect(resultRows).toEqual(expected);
  }, 10000);
});
