import { describe, expect, it } from "vitest";
import type { FindManyQuery, Schema } from "@repo/types";
import { assertQueryReferencesAllowedTables } from "./index";

function createTable(name: string): Schema[number] {
  return {
    name,
    schema: "public",
    access: "QUERYABLE",
    context: null,
    columns: [
      {
        dbName: "id",
        name: "id",
        rename: null,
        notes: null,
        type: "number",
        effectiveType: "number",
        conversion: null,
        enumMode: null,
        valueEnum: null,
        unit: null,
        relation: [],
      },
    ],
    computedColumns: [],
    outwardRelations: [],
    condition: null,
    accessPolicy: null,
  };
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

describe("assertQueryReferencesAllowedTables", () => {
  it("allows a query when all referenced tables are present", () => {
    const schema: Schema = [createTable("users"), createTable("billing")];
    const query: FindManyQuery = {
      ...createFindManyQuery(),
      operationParameters: {
        select: {
          users: ["id"],
        },
        joins: [
          {
            table: "billing",
            path: [
              {
                source: ["users.id"],
                target: ["billing.id"],
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
    const schema: Schema = [createTable("billing")];
    const query = createFindManyQuery();

    expect(() => assertQueryReferencesAllowedTables(query, schema)).toThrow(
      "users",
    );
  });

  it("rejects when a joined table is not in runtime schema", () => {
    const schema: Schema = [createTable("users")];
    const query: FindManyQuery = {
      ...createFindManyQuery(),
      operationParameters: {
        select: {
          users: ["id"],
        },
        joins: [
          {
            table: "billing",
            path: [
              {
                source: ["users.id"],
                target: ["billing.id"],
              },
            ],
          },
        ],
        orderBy: null,
        limit: 50,
      },
    };

    expect(() => assertQueryReferencesAllowedTables(query, schema)).toThrow(
      "billing",
    );
  });

  it("rejects when join path references a table not in runtime schema", () => {
    const schema: Schema = [createTable("users"), createTable("billing")];
    const query: FindManyQuery = {
      ...createFindManyQuery(),
      operationParameters: {
        select: {
          users: ["id"],
        },
        joins: [
          {
            table: "billing",
            path: [
              {
                source: ["users.id"],
                target: ["billing.id"],
              },
              {
                source: ["billing.id"],
                target: ["admin_audit.billing_id"],
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
