import { describe, it, expect } from "vitest";
import {
  buildAggregateGroupsZodSchema,
  validateAggregateGroupsCandidate,
} from "./aggregateGroupsValidator";

const ctx = {
  baseTableName: "users",
  joinOptions: [
    {
      name: "users.orders",
      table: "orders",
      path: [
        {
          source: ["users.id"],
          target: ["orders.user_id"],
        },
      ],
    },
  ],
  allColumns: [
    "users.id",
    "users.name",
    "orders.id",
    "orders.total",
    "users.orders.id",
    "users.orders.total",
    "users.createdAt",
  ],
  groupableColumns: [
    "users.id",
    "users.name",
    "orders.id",
    "orders.total",
    "users.orders.id",
    "users.orders.total",
  ],
  intervalColumns: ["users.createdAt"],
  numericalColumns: ["orders.total", "users.orders.total"],
};

const productCtx = {
  baseTableName: "orders",
  joinOptions: [
    {
      name: "product",
      table: "products",
      path: [
        {
          source: ["orders.product_id"],
          target: ["products.id"],
        },
      ],
    },
  ],
  allColumns: [
    "orders.id",
    "orders.total",
    "orders.created_at",
    "product.id",
    "product.title",
    "product.category",
    "product.created_at",
  ],
  groupableColumns: [
    "orders.id",
    "orders.total",
    "product.id",
    "product.title",
    "product.category",
  ],
  intervalColumns: ["orders.created_at", "product.created_at"],
  numericalColumns: ["orders.total"],
};

describe("aggregateGroups validator", () => {
  it("accepts valid candidate and applies default reducers", () => {
    const schema = buildAggregateGroupsZodSchema(ctx);
    const candidate = {
      joins: [
        {
          table: "orders",
          name: "users.orders",
          path: [
            {
              source: ["users.id"],
              target: ["orders.user_id"],
            },
          ],
        },
      ],
      groupBy: [{ type: "column", column: "users.id" }],
      having: [
        {
          type: "aggregate" as const,
          function: "count" as const,
          column: "orders.id",
          operator: "gt" as const,
          value: 2,
        },
      ],
      aggregates: {
        groupCount: true,
        count: ["orders.id"],
        sum: ["orders.total"],
      },
    };
    const parsed = schema.safeParse(candidate);
    expect(parsed.success).toBe(true);
    const result = validateAggregateGroupsCandidate(candidate, ctx);
    expect(result.status).toBe("valid");
    if (result.status === "valid") {
      expect(result.result.aggregates.count).toEqual(["orders.id"]);
      expect(result.result.reducers?.count).toEqual(["sum"]);
      expect(result.result.reducers?.sum).toEqual(["sum"]);
    }
  });

  it("accepts groupBy keys qualified by join alias", () => {
    const candidate = {
      joins: [
        {
          table: "orders",
          name: "users.orders",
          path: [
            {
              source: ["users.id"],
              target: ["orders.user_id"],
            },
          ],
        },
      ],
      groupBy: [{ type: "column" as const, column: "users.orders.id" }],
      having: null,
      aggregates: {
        groupCount: true,
        sum: ["users.orders.total"],
      },
    };

    const result = validateAggregateGroupsCandidate(candidate, ctx);
    expect(result.status).toBe("valid");
  });

  it("suggests valid alias-scoped columns for an invalid joined groupBy column", () => {
    const candidate = {
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
      groupBy: [{ type: "column" as const, column: "product.name" }],
      having: null,
      aggregates: {
        groupCount: true,
        sum: ["orders.total"],
      },
    };

    const result = validateAggregateGroupsCandidate(candidate, productCtx);
    expect(result.status).toBe("invalid");
    if (result.status === "invalid") {
      expect(result.issues).toContainEqual(
        expect.objectContaining({
          path: "groupBy.0",
          code: "invalid_group_by_key",
        }),
      );
      expect(result.issues[0]?.message).toContain(
        "Valid groupBy columns for product",
      );
      expect(result.issues[0]?.message).toContain("product.title");
    }
  });

  it("rejects reducers without matching aggregates", () => {
    const candidate = {
      joins: null,
      groupBy: [{ type: "column", column: "users.id" }],
      having: null,
      aggregates: { groupCount: true },
      reducers: { count: ["sum"] },
    };
    const result = validateAggregateGroupsCandidate(candidate, ctx);
    expect(result.status).toBe("invalid");
  });

  it("rejects missing aggregates when groupCount is false", () => {
    const candidate = {
      joins: null,
      groupBy: [{ type: "column", column: "users.id" }],
      having: null,
      aggregates: { groupCount: false },
    };
    const result = validateAggregateGroupsCandidate(candidate, ctx);
    expect(result.status).toBe("invalid");
  });

  it("rejects HAVING aggregate on non-numeric column for numeric function", () => {
    const candidate = {
      joins: null,
      groupBy: [{ type: "column", column: "users.id" }],
      having: [
        {
          type: "aggregate" as const,
          function: "sum" as const,
          column: "users.name",
          operator: "gt" as const,
          value: 1,
        },
      ],
      aggregates: { groupCount: true },
    };
    const result = validateAggregateGroupsCandidate(candidate, ctx);
    expect(result.status).toBe("invalid");
  });

  it("rejects HAVING dateInterval groupKey with relative date expression", () => {
    const candidate = {
      joins: null,
      groupBy: [
        {
          type: "dateInterval" as const,
          column: "users.createdAt",
          interval: "month" as const,
          alias: "month",
        },
      ],
      having: [
        {
          type: "groupKey" as const,
          key: "month",
          operator: "gte" as const,
          value: "now()-30d", // Invalid - should be bucket format
        },
      ],
      aggregates: { groupCount: true },
    };
    const result = validateAggregateGroupsCandidate(candidate, ctx);
    expect(result.status).toBe("invalid");
    if (result.status === "invalid") {
      expect(result.issues[0]?.code).toBe("invalid_date_interval_value");
      expect(result.issues[0]?.message).toContain("2024-01");
    }
  });

  it("accepts HAVING dateInterval groupKey with valid month bucket format", () => {
    const candidate = {
      joins: null,
      groupBy: [
        {
          type: "dateInterval" as const,
          column: "users.createdAt",
          interval: "month" as const,
          alias: "month",
        },
      ],
      having: [
        {
          type: "groupKey" as const,
          key: "month",
          operator: "gte" as const,
          value: "2026-01", // Valid month bucket format
        },
      ],
      aggregates: { groupCount: true },
    };
    const result = validateAggregateGroupsCandidate(candidate, ctx);
    expect(result.status).toBe("valid");
    if (result.status === "valid") {
      expect(result.result.having?.[0]?.value).toBe("2026-01");
    }
  });

  it("rejects HAVING dateComponent groupKey with invalid string value", () => {
    const candidate = {
      joins: null,
      groupBy: [
        {
          type: "dateComponent" as const,
          column: "users.createdAt",
          component: "monthOfYear" as const,
          alias: "month",
        },
      ],
      having: [
        {
          type: "groupKey" as const,
          key: "month",
          operator: "equals" as const,
          value: "April", // Invalid - should be 1-12
        },
      ],
      aggregates: { groupCount: true },
    };
    const result = validateAggregateGroupsCandidate(candidate, ctx);
    expect(result.status).toBe("invalid");
    if (result.status === "invalid") {
      expect(result.issues[0]?.code).toBe("invalid_date_component_value");
      expect(result.issues[0]?.message).toContain("month (1-12)");
    }
  });

  it("accepts HAVING dateComponent groupKey with valid numeric value", () => {
    const candidate = {
      joins: null,
      groupBy: [
        {
          type: "dateComponent" as const,
          column: "users.createdAt",
          component: "dayOfWeek" as const,
          alias: "dow",
        },
      ],
      having: [
        {
          type: "groupKey" as const,
          key: "dow",
          operator: "in" as const,
          value: [1, 2, 3, 4, 5], // Valid - weekdays (1-5)
        },
      ],
      aggregates: { groupCount: true },
    };
    const result = validateAggregateGroupsCandidate(candidate, ctx);
    expect(result.status).toBe("valid");
  });

  it("rejects HAVING dateInterval groupKey in operator with mixed valid/invalid bucket formats", () => {
    const candidate = {
      joins: null,
      groupBy: [
        {
          type: "dateInterval" as const,
          column: "users.createdAt",
          interval: "day" as const,
          alias: "day",
        },
      ],
      having: [
        {
          type: "groupKey" as const,
          key: "day",
          operator: "in" as const,
          value: ["2026-01-01", "last week"], // Second value is invalid
        },
      ],
      aggregates: { groupCount: true },
    };
    const result = validateAggregateGroupsCandidate(candidate, ctx);
    expect(result.status).toBe("invalid");
    if (result.status === "invalid") {
      expect(result.issues[0]?.code).toBe("invalid_date_interval_value");
    }
  });
});
