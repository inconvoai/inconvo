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
    "users.createdAt",
  ],
  groupableColumns: ["users.id", "users.name", "orders.id", "orders.total"],
  intervalColumns: ["users.createdAt"],
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
});
