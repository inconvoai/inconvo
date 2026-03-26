import { describe, expect, it, vi } from "vitest";
import type {
  DatabaseConnector,
  FindManyQuery,
  Query,
  QueryResponse,
  Schema,
} from "@repo/types";
import { databaseRetrieverQueryDraftSchema } from "./queryDraft";
import {
  assertQueryReferencesAllowedTables,
  databaseRetrieverAgent,
} from "./index";

function createColumn(
  name: string,
  type: string,
  overrides: Partial<Schema[number]["columns"][number]> = {},
): Schema[number]["columns"][number] {
  return {
    dbName: name,
    name,
    rename: null,
    notes: null,
    type,
    effectiveType: type,
    conversion: null,
    enumMode: null,
    valueEnum: null,
    unit: null,
    relation: [],
    ...overrides,
  };
}

function createRelation(
  relation: Partial<Schema[number]["outwardRelations"][number]> & {
    name: string;
    targetTableName: string;
    isList: boolean;
    sourceColumns: string[];
    targetColumns: string[];
  },
): Schema[number]["outwardRelations"][number] {
  return {
    name: relation.name,
    relationId: null,
    targetTable: { name: relation.targetTableName },
    targetSchema: null,
    isList: relation.isList,
    selected: true,
    source: "FK",
    status: "VALID",
    errorTag: null,
    sourceColumns: relation.sourceColumns,
    targetColumns: relation.targetColumns,
  };
}

function createSchema(access: Schema[number]["access"] = "QUERYABLE"): Schema {
  return [
    {
      name: "users",
      schema: "public",
      access: "QUERYABLE",
      context: null,
      summary: null,
      columns: [
        createColumn("id", "number"),
        createColumn("name", "string"),
        createColumn("organisation_id", "number"),
      ],
      computedColumns: [],
      outwardRelations: [
        createRelation({
          name: "orders",
          targetTableName: "orders",
          isList: true,
          sourceColumns: ["id"],
          targetColumns: ["user_id"],
        }),
      ],
      condition: {
        column: { name: "organisation_id" },
        userContextField: { key: "organisationId" },
      },
      accessPolicy: null,
    },
    {
      name: "orders",
      schema: "public",
      access,
      context: null,
      summary: null,
      columns: [
        createColumn("id", "number"),
        createColumn("status", "string"),
        createColumn("total", "number"),
        createColumn("created_at", "DateTime"),
        createColumn("user_id", "number"),
        createColumn("organisation_id", "number"),
      ],
      computedColumns: [],
      outwardRelations: [
        createRelation({
          name: "customer",
          targetTableName: "users",
          isList: false,
          sourceColumns: ["user_id"],
          targetColumns: ["id"],
        }),
      ],
      condition: {
        column: { name: "organisation_id" },
        userContextField: { key: "organisationId" },
      },
      accessPolicy: null,
    },
  ];
}

function createFindManyQuery(): FindManyQuery {
  return {
    table: "users",
    tableSchema: "public",
    whereAndArray: [],
    tableConditions: null,
    operation: "findMany",
    operationParameters: {
      select: {
        users: ["id"],
      },
      joins: null,
      orderBy: null,
      limit: 50,
    },
  };
}

function createMockConnector(
  implementation?: (query: Query) => Promise<QueryResponse>,
): DatabaseConnector & { query: ReturnType<typeof vi.fn> } {
  return {
    query: vi.fn(
      implementation ??
        (async (query: Query) => ({
          query: {
            sql: `select * from ${query.table}`,
            params: [],
          },
          data: [],
        })),
    ),
  };
}

describe("databaseRetrieverQueryDraftSchema", () => {
  it("accepts the new structured query draft", () => {
    const result = databaseRetrieverQueryDraftSchema.safeParse({
      table: "orders",
      operation: "count",
      operationParameters: {
        joins: null,
        count: ["orders.id"],
        countDistinct: null,
      },
      questionConditions: null,
    });

    expect(result.success).toBe(true);
  });

  it("defaults missing questionConditions to null", () => {
    const result = databaseRetrieverQueryDraftSchema.safeParse({
      table: "orders",
      operation: "count",
      operationParameters: {
        joins: null,
        count: ["orders.id"],
        countDistinct: null,
      },
    });

    expect(result.success).toBe(true);
    if (result.success) {
      expect(result.data.questionConditions).toBeNull();
    }
  });

  it("rejects the legacy natural-language payload shape", () => {
    const result = databaseRetrieverQueryDraftSchema.safeParse({
      table: "orders",
      operation: "count",
      question: "How many orders do we have?",
      questionConditions: null,
    });

    expect(result.success).toBe(false);
  });
});

describe("databaseRetrieverAgent", () => {
  it("applies context conditions to the base and joined tables", async () => {
    const connector = createMockConnector();
    const graph = await databaseRetrieverAgent({
      query: {
        table: "orders",
        operation: "findMany",
        operationParameters: {
          select: {
            orders: ["id", "status"],
            "orders.customer": ["name"],
          },
          joins: [
            {
              table: "users",
              name: "orders.customer",
              path: [
                {
                  source: ["orders.user_id"],
                  target: ["users.id"],
                },
              ],
            },
          ],
          orderBy: null,
          limit: 10,
        },
        questionConditions: null,
      },
      schema: createSchema(),
      userContext: { organisationId: 42 },
      connector,
    });

    const result = await graph.invoke({});

    expect(result.error).toBeUndefined();
    expect(result.joinedTableNames).toEqual(["users"]);
    expect(connector.query).toHaveBeenCalledTimes(1);

    const executedQuery = connector.query.mock.calls[0]?.[0] as Query;
    expect(executedQuery.whereAndArray).toEqual([
      {
        "orders.organisation_id": { equals: 42 },
        "users.organisation_id": { equals: 42 },
      },
    ]);
    expect(executedQuery.tableConditions).toEqual({
      orders: { column: "organisation_id", value: 42 },
      users: { column: "organisation_id", value: 42 },
    });
  });

  it("returns a validation error when questionConditions are invalid", async () => {
    const connector = createMockConnector();
    const graph = await databaseRetrieverAgent({
      query: {
        table: "orders",
        operation: "count",
        operationParameters: {
          joins: null,
          count: ["orders.id"],
          countDistinct: null,
        },
        questionConditions: {
          AND: [
            {
              "unknown_table.status": {
                equals: "paid",
              },
            },
          ],
        },
      },
      schema: createSchema(),
      userContext: { organisationId: 42 },
      connector,
    });

    const result = await graph.invoke({});

    expect(result.error).toMatchObject({
      type: "validation",
      stage: "questionConditions",
    });
    expect(connector.query).not.toHaveBeenCalled();
  });

  it("rejects a non-queryable base table before execution", async () => {
    const connector = createMockConnector();
    const graph = await databaseRetrieverAgent({
      query: {
        table: "orders",
        operation: "count",
        operationParameters: {
          joins: null,
          count: ["orders.id"],
          countDistinct: null,
        },
        questionConditions: null,
      },
      schema: createSchema("JOINABLE"),
      userContext: { organisationId: 42 },
      connector,
    });

    const result = await graph.invoke({});

    expect(result.error).toMatchObject({
      type: "validation",
      stage: "table",
    });
    expect(connector.query).not.toHaveBeenCalled();
  });
});

describe("assertQueryReferencesAllowedTables", () => {
  it("allows a query when all referenced tables are present", () => {
    const schema: Schema = createSchema();
    const query: FindManyQuery = {
      ...createFindManyQuery(),
      operationParameters: {
        select: {
          users: ["id"],
        },
        joins: [
          {
            table: "orders",
            path: [
              {
                source: ["users.id"],
                target: ["orders.user_id"],
              },
            ],
          },
        ],
        orderBy: null,
        limit: 50,
      },
    };

    expect(() => assertQueryReferencesAllowedTables(query, schema)).not.toThrow();
  });

  it("rejects when the base table is not in runtime schema", () => {
    const schema: Schema = [createSchema()[1]!];
    const query = createFindManyQuery();

    expect(() => assertQueryReferencesAllowedTables(query, schema)).toThrow(
      "users",
    );
  });

  it("rejects when a joined table is not in runtime schema", () => {
    const schema: Schema = [createSchema()[0]!];
    const query: FindManyQuery = {
      ...createFindManyQuery(),
      operationParameters: {
        select: {
          users: ["id"],
        },
        joins: [
          {
            table: "orders",
            path: [
              {
                source: ["users.id"],
                target: ["orders.user_id"],
              },
            ],
          },
        ],
        orderBy: null,
        limit: 50,
      },
    };

    expect(() => assertQueryReferencesAllowedTables(query, schema)).toThrow(
      "orders",
    );
  });

  it("rejects when join path references a table not in runtime schema", () => {
    const schema: Schema = createSchema();
    const query: FindManyQuery = {
      ...createFindManyQuery(),
      operationParameters: {
        select: {
          users: ["id"],
        },
        joins: [
          {
            table: "orders",
            path: [
              {
                source: ["users.id"],
                target: ["orders.user_id"],
              },
              {
                source: ["orders.id"],
                target: ["admin_audit.order_id"],
              },
            ],
          },
        ],
        orderBy: null,
        limit: 50,
      },
    };

    expect(() => assertQueryReferencesAllowedTables(query, schema)).toThrow(
      "admin_audit",
    );
  });
});
