import { describe, it, expect } from "vitest";
import {
  buildGroupByZodSchema,
  validateGroupByCandidate,
} from "./groupByValidator";

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
    {
      name: "users.profiles",
      table: "profiles",
      path: [
        {
          source: ["users.id"],
          target: ["profiles.user_id"],
        },
      ],
    },
  ],
  allColumns: [
    "users.id",
    "users.name",
    "orders.id",
    "orders.total",
    "profiles.id",
    "profiles.age",
    "users.createdAt",
  ],
  groupableColumns: [
    "users.id",
    "users.name",
    "orders.id",
    "orders.total",
    "profiles.id",
    "profiles.age",
  ],
  intervalColumns: ["users.createdAt"],
  numericalColumns: ["orders.total", "profiles.age"],
};

describe("groupBy validator", () => {
  it("valid candidate passes and strips empty joins", () => {
    const schema = buildGroupByZodSchema(ctx);
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
          joinType: "left" as const,
        },
      ],
      groupBy: [{ type: "column", column: "users.id" }],
      count: ["users.id"],
      countDistinct: null,
      sum: ["orders.total"],
      min: null,
      max: null,
      avg: null,
      orderBy: {
        type: "aggregate",
        function: "count",
        column: "users.id",
        direction: "asc",
      },
      limit: 10,
    };
    const parsed = schema.safeParse(candidate);
    expect(parsed.success).toBe(true);
    const result = validateGroupByCandidate(candidate, ctx);
    expect(result.status).toBe("valid");
    if (result.status === "valid") {
      expect(result.result.groupBy).toEqual([
        { type: "column", column: "users.id", alias: "users.id" },
      ]);
      expect(result.result.count).toEqual(["users.id"]);
    }
  });

  it("invalid when orderBy count column not in groupBy", () => {
    const bad = {
      joins: null,
      groupBy: [{ type: "column", column: "users.id" }],
      count: ["users.id"],
      countDistinct: null,
      sum: null,
      min: null,
      max: null,
      avg: null,
      orderBy: {
        type: "aggregate",
        function: "count",
        column: "users.name",
        direction: "asc",
      },
      limit: 5,
    };
    const result = validateGroupByCandidate(bad, ctx);
    expect(result.status).toBe("invalid");
  });

  it("invalid when numeric function uses non-numeric column", () => {
    const bad = {
      joins: null,
      groupBy: [{ type: "column", column: "users.id" }],
      count: null,
      countDistinct: null,
      sum: ["users.name"], // not numeric
      min: null,
      max: null,
      avg: null,
      orderBy: {
        type: "aggregate",
        function: "sum",
        column: "users.name",
        direction: "asc",
      },
      limit: 5,
    };
    const result = validateGroupByCandidate(bad, ctx);
    expect(result.status).toBe("invalid");
  });

  it("invalid when grouping by temporal column without interval", () => {
    const bad = {
      joins: null,
      groupBy: [{ type: "column", column: "users.createdAt" }],
      count: null,
      countDistinct: null,
      sum: null,
      min: null,
      max: null,
      avg: null,
      orderBy: {
        type: "aggregate" as const,
        function: "count" as const,
        column: "users.id",
        direction: "asc" as const,
      },
      limit: 5,
    };
    const result = validateGroupByCandidate(bad, ctx);
    expect(result.status).toBe("invalid");
  });

  it("valid interval grouping uses alias defaults", () => {
    const candidate = {
      joins: null,
      groupBy: [
        {
          type: "dateInterval" as const,
          column: "users.createdAt",
          interval: "month" as const,
        },
      ],
      count: null,
      countDistinct: null,
      sum: null,
      min: null,
      max: null,
      avg: null,
      orderBy: {
        type: "groupKey" as const,
        key: "users.createdAt|month",
        direction: "asc" as const,
      },
      limit: 5,
    };
    const result = validateGroupByCandidate(candidate, ctx);
    expect(result.status).toBe("valid");
    if (result.status === "valid") {
      expect(result.result.groupBy[0]?.alias).toBe("users.createdAt|month");
    }
  });

  it("valid component grouping uses alias defaults", () => {
    const candidate = {
      joins: null,
      groupBy: [
        {
          type: "dateComponent" as const,
          column: "users.createdAt",
          component: "dayOfWeek" as const,
        },
      ],
      count: null,
      countDistinct: null,
      sum: null,
      min: null,
      max: null,
      avg: null,
      orderBy: {
        type: "groupKey" as const,
        key: "users.createdAt|dayOfWeek",
        direction: "asc" as const,
      },
      limit: 7,
    };
    const result = validateGroupByCandidate(candidate, ctx);
    expect(result.status).toBe("valid");
    if (result.status === "valid") {
      expect(result.result.groupBy[0]?.alias).toBe("users.createdAt|dayOfWeek");
    }
  });

  it("invalid when dateComponent targets non-temporal column", () => {
    const bad = {
      joins: null,
      groupBy: [
        {
          type: "dateComponent" as const,
          column: "users.name",
          component: "monthOfYear" as const,
        },
      ],
      count: null,
      countDistinct: null,
      sum: null,
      min: null,
      max: null,
      avg: null,
      orderBy: {
        type: "groupKey" as const,
        key: "users.name|monthOfYear",
        direction: "asc" as const,
      },
      limit: 12,
    };
    const result = validateGroupByCandidate(bad, ctx);
    expect(result.status).toBe("invalid");
  });

  it("validates countDistinct aggregation", () => {
    const candidate = {
      joins: null,
      groupBy: [{ type: "column", column: "users.id" }],
      count: null,
      countDistinct: ["users.name"],
      sum: null,
      min: null,
      max: null,
      avg: null,
      orderBy: {
        type: "aggregate" as const,
        function: "countDistinct" as const,
        column: "users.name",
        direction: "desc" as const,
      },
      limit: 10,
    };
    const result = validateGroupByCandidate(candidate, ctx);
    expect(result.status).toBe("valid");
    if (result.status === "valid") {
      expect(result.result.countDistinct).toEqual(["users.name"]);
    }
  });

  it("accepts HAVING clauses on aggregates and group keys", () => {
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
      groupBy: [{ type: "column", column: "users.id", alias: "userId" }],
      count: null,
      countDistinct: null,
      sum: ["orders.total"],
      min: null,
      max: null,
      avg: null,
      orderBy: {
        type: "groupKey" as const,
        key: "userId",
        direction: "asc" as const,
      },
      having: [
        {
          type: "aggregate" as const,
          function: "count" as const,
          column: "orders.id",
          operator: "gte" as const,
          value: 2,
        },
        {
          type: "aggregate" as const,
          function: "sum" as const,
          column: "orders.total",
          operator: "gt" as const,
          value: 100,
        },
        {
          type: "groupKey" as const,
          key: "userId",
          operator: "not" as const,
          value: null,
        },
      ],
      limit: 5,
    };
    const result = validateGroupByCandidate(candidate, ctx);
    expect(result.status).toBe("valid");
    if (result.status === "valid") {
      expect(result.result.having?.length).toBe(3);
    }
  });

  it("rejects HAVING groupKey entries that do not match aliases", () => {
    const candidate = {
      joins: null,
      groupBy: [{ type: "column", column: "users.id" }],
      count: null,
      countDistinct: null,
      sum: null,
      min: null,
      max: null,
      avg: null,
      orderBy: {
        type: "groupKey" as const,
        key: "users.id",
        direction: "asc" as const,
      },
      having: [
        {
          type: "groupKey" as const,
          key: "missing",
          operator: "equals" as const,
          value: "x",
        },
      ],
      limit: 3,
    };
    const result = validateGroupByCandidate(candidate, ctx);
    expect(result.status).toBe("invalid");
  });

  it("rejects HAVING aggregates on unselected join tables", () => {
    const candidate = {
      joins: null,
      groupBy: [{ type: "column", column: "users.id" }],
      count: null,
      countDistinct: null,
      sum: null,
      min: null,
      max: null,
      avg: null,
      orderBy: {
        type: "groupKey" as const,
        key: "users.id",
        direction: "asc" as const,
      },
      having: [
        {
          type: "aggregate" as const,
          function: "count" as const,
          column: "orders.id",
          operator: "gt" as const,
          value: 1,
        },
      ],
      limit: 5,
    };
    const result = validateGroupByCandidate(candidate, ctx);
    expect(result.status).toBe("invalid");
  });

  it("rejects HAVING numeric functions on non-numeric columns", () => {
    const candidate = {
      joins: null,
      groupBy: [{ type: "column", column: "users.id" }],
      count: null,
      countDistinct: null,
      sum: null,
      min: null,
      max: null,
      avg: null,
      orderBy: {
        type: "groupKey" as const,
        key: "users.id",
        direction: "asc" as const,
      },
      having: [
        {
          type: "aggregate" as const,
          function: "sum" as const,
          column: "users.name",
          operator: "gt" as const,
          value: 1,
        },
      ],
      limit: 5,
    };
    const result = validateGroupByCandidate(candidate, ctx);
    expect(result.status).toBe("invalid");
  });

  it("rejects HAVING groupKey with invalid monthOfYear value (string month name)", () => {
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
      count: ["users.id"],
      countDistinct: null,
      sum: null,
      min: null,
      max: null,
      avg: null,
      orderBy: {
        type: "groupKey" as const,
        key: "month",
        direction: "asc" as const,
      },
      having: [
        {
          type: "groupKey" as const,
          key: "month",
          operator: "equals" as const,
          value: "April", // Invalid - should be 1-12
        },
      ],
      limit: 12,
    };
    const result = validateGroupByCandidate(candidate, ctx);
    expect(result.status).toBe("invalid");
    if (result.status === "invalid") {
      expect(result.issues[0]?.code).toBe("invalid_date_component_value");
      expect(result.issues[0]?.message).toContain("month (1-12)");
    }
  });

  it("rejects HAVING groupKey with invalid monthOfYear value in array", () => {
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
      count: ["users.id"],
      countDistinct: null,
      sum: null,
      min: null,
      max: null,
      avg: null,
      orderBy: {
        type: "groupKey" as const,
        key: "month",
        direction: "asc" as const,
      },
      having: [
        {
          type: "groupKey" as const,
          key: "month",
          operator: "in" as const,
          value: ["2023", "2024"], // Invalid - should be 1-12
        },
      ],
      limit: 12,
    };
    const result = validateGroupByCandidate(candidate, ctx);
    expect(result.status).toBe("invalid");
    if (result.status === "invalid") {
      expect(result.issues[0]?.code).toBe("invalid_date_component_value");
    }
  });

  it("accepts HAVING groupKey with valid monthOfYear numeric values", () => {
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
      count: ["users.id"],
      countDistinct: null,
      sum: null,
      min: null,
      max: null,
      avg: null,
      orderBy: {
        type: "groupKey" as const,
        key: "month",
        direction: "asc" as const,
      },
      having: [
        {
          type: "groupKey" as const,
          key: "month",
          operator: "in" as const,
          value: [4, 5, 6], // Valid - Q2 months
        },
      ],
      limit: 12,
    };
    const result = validateGroupByCandidate(candidate, ctx);
    expect(result.status).toBe("valid");
  });

  it("accepts HAVING groupKey with valid numeric string values for monthOfYear", () => {
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
      count: ["users.id"],
      countDistinct: null,
      sum: null,
      min: null,
      max: null,
      avg: null,
      orderBy: {
        type: "groupKey" as const,
        key: "month",
        direction: "asc" as const,
      },
      having: [
        {
          type: "groupKey" as const,
          key: "month",
          operator: "equals" as const,
          value: "4", // Valid - numeric string for April
        },
      ],
      limit: 12,
    };
    const result = validateGroupByCandidate(candidate, ctx);
    expect(result.status).toBe("valid");
  });

  it("rejects HAVING groupKey with out-of-range dayOfWeek value", () => {
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
      count: ["users.id"],
      countDistinct: null,
      sum: null,
      min: null,
      max: null,
      avg: null,
      orderBy: {
        type: "groupKey" as const,
        key: "dow",
        direction: "asc" as const,
      },
      having: [
        {
          type: "groupKey" as const,
          key: "dow",
          operator: "equals" as const,
          value: 8, // Invalid - should be 1-7
        },
      ],
      limit: 7,
    };
    const result = validateGroupByCandidate(candidate, ctx);
    expect(result.status).toBe("invalid");
    if (result.status === "invalid") {
      expect(result.issues[0]?.code).toBe("invalid_date_component_value");
      expect(result.issues[0]?.message).toContain("day of week (1-7)");
    }
  });
});
