import { describe, expect, it } from "vitest";
import type { Schema } from "@repo/types";
import type { SupportedDatabaseRetrieverOperation } from "../queryDraft";
import { validateOperationParametersDraft } from "./validateOperationParametersDraft";

function createColumn(
  name: string,
  type: string,
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
  };
}

function createRelation(
  name: string,
  targetTableName: string,
  isList: boolean,
  sourceColumns: string[],
  targetColumns: string[],
): Schema[number]["outwardRelations"][number] {
  return {
    name,
    relationId: null,
    targetTable: { name: targetTableName },
    targetSchema: null,
    isList,
    selected: true,
    source: "FK",
    status: "VALID",
    errorTag: null,
    sourceColumns,
    targetColumns,
  };
}

function createSchema(): Schema {
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
        createRelation("orders", "orders", true, ["id"], ["user_id"]),
      ],
      condition: null,
      accessPolicy: null,
    },
    {
      name: "orders",
      schema: "public",
      access: "QUERYABLE",
      context: null,
      summary: null,
      columns: [
        createColumn("id", "number"),
        createColumn("status", "string"),
        createColumn("total", "number"),
        createColumn("created_at", "DateTime"),
        createColumn("user_id", "number"),
        createColumn("product_id", "number"),
      ],
      computedColumns: [],
      outwardRelations: [
        createRelation("customer", "users", false, ["user_id"], ["id"]),
        createRelation("product", "products", false, ["product_id"], ["id"]),
      ],
      condition: null,
      accessPolicy: null,
    },
    {
      name: "products",
      schema: "public",
      access: "QUERYABLE",
      context: null,
      summary: null,
      columns: [
        createColumn("id", "number"),
        createColumn("category", "string"),
      ],
      computedColumns: [],
      outwardRelations: [],
      condition: null,
      accessPolicy: null,
    },
  ];
}

const schema = createSchema();

const validCases: Array<{
  operation: SupportedDatabaseRetrieverOperation;
  tableName: string;
  candidateOperationParameters: Record<string, unknown>;
}> = [
  {
    operation: "findMany",
    tableName: "orders",
    candidateOperationParameters: {
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
  },
  {
    operation: "findDistinct",
    tableName: "orders",
    candidateOperationParameters: {
      column: "orders.status",
    },
  },
  {
    operation: "count",
    tableName: "orders",
    candidateOperationParameters: {
      joins: null,
      count: ["orders.id"],
      countDistinct: null,
    },
  },
  {
    operation: "count",
    tableName: "orders",
    candidateOperationParameters: {
      joins: [
        {
          table: "products",
          name: "product",
          path: [
            {
              source: ["orders.product_id"],
              target: ["products.id"],
            },
          ],
        },
      ],
      count: ["product.id"],
      countDistinct: ["product.category"],
    },
  },
  {
    operation: "countRelations",
    tableName: "users",
    candidateOperationParameters: {
      columns: ["id"],
      joins: [
        {
          name: "orders",
          table: "orders",
          path: [
            {
              source: ["users.id"],
              target: ["orders.user_id"],
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
      limit: 10,
    },
  },
  {
    operation: "aggregate",
    tableName: "orders",
    candidateOperationParameters: {
      joins: null,
      avg: null,
      sum: ["orders.total"],
      min: null,
      max: null,
      count: ["orders.id"],
      countDistinct: null,
      median: null,
    },
  },
  {
    operation: "aggregate",
    tableName: "orders",
    candidateOperationParameters: {
      joins: [
        {
          table: "products",
          name: "product",
          path: [
            {
              source: ["orders.product_id"],
              target: ["products.id"],
            },
          ],
          joinType: "inner",
        },
      ],
      avg: null,
      sum: ["orders.total"],
      min: null,
      max: null,
      count: ["product.id"],
      countDistinct: ["product.category"],
      median: null,
    },
  },
  {
    operation: "aggregateGroups",
    tableName: "orders",
    candidateOperationParameters: {
      joins: null,
      groupBy: [
        {
          type: "column",
          column: "orders.status",
          alias: "status",
        },
      ],
      having: null,
      aggregates: {
        groupCount: true,
        count: null,
        countDistinct: null,
        sum: null,
        min: null,
        max: null,
        avg: null,
      },
    },
  },
  {
    operation: "groupBy",
    tableName: "orders",
    candidateOperationParameters: {
      joins: null,
      groupBy: [
        {
          type: "column",
          column: "orders.status",
          alias: "status",
        },
      ],
      count: ["orders.id"],
      countDistinct: null,
      sum: null,
      min: null,
      max: null,
      avg: null,
      orderBy: {
        type: "groupKey",
        key: "status",
        direction: "asc",
      },
      limit: 10,
      having: null,
    },
  },
  {
    operation: "groupBy",
    tableName: "orders",
    candidateOperationParameters: {
      joins: [
        {
          table: "products",
          name: "product",
          path: [
            {
              source: ["orders.product_id"],
              target: ["products.id"],
            },
          ],
          joinType: "inner",
        },
      ],
      groupBy: [
        {
          type: "column",
          column: "product.category",
        },
      ],
      count: ["orders.id"],
      countDistinct: null,
      sum: ["orders.total"],
      min: null,
      max: null,
      avg: null,
      orderBy: {
        type: "aggregate",
        function: "sum",
        column: "orders.total",
        direction: "desc",
      },
      limit: 20,
      having: null,
    },
  },
];

describe("validateOperationParametersDraft", () => {
  it.each(validCases)(
    "returns normalized success for $operation",
    ({ operation, tableName, candidateOperationParameters }) => {
      const tableSchema = schema.find((table) => table.name === tableName)!;
      const result = validateOperationParametersDraft({
        operation,
        candidateOperationParameters,
        schema,
        tableName,
        tableSchema,
      });

      expect(result.status).toBe("valid");
      if (result.status === "valid") {
        expect(result.operationParameters).toBeDefined();
      }
    },
  );

  it.each(validCases)(
    "returns invalid for malformed $operation parameters",
    ({ operation, tableName }) => {
      const tableSchema = schema.find((table) => table.name === tableName)!;
      const result = validateOperationParametersDraft({
        operation,
        candidateOperationParameters: {},
        schema,
        tableName,
        tableSchema,
      });

      expect(result.status).toBe("invalid");
      if (result.status === "invalid") {
        expect(result.issues.length).toBeGreaterThan(0);
      }
    },
  );
});
