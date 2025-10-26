import { describe, it, expect } from "vitest";
import {
  buildGroupByZodSchema,
  validateGroupByCandidate,
} from "./groupByValidator";

const ctx = {
  baseTableName: "users",
  joinOptions: [
    { table: "orders", joinPath: "users>orders" },
    { table: "profiles", joinPath: "users>profiles" },
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
      joins: [{ table: "orders", joinPath: "users>orders", joinType: "left" }],
      groupBy: [{ type: "column", column: "users.id" }],
      count: ["users.id"],
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
});
