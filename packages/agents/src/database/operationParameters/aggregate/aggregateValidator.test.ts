import { describe, expect, it } from "vitest";
import {
  validateAggregateCandidate,
  type AggregateValidatorContext,
  type AggregateColumnCatalog,
} from "./aggregateValidator";

const columnCatalog: AggregateColumnCatalog = {
  users: {
    id: { isNumeric: false, isTemporal: false, isCountable: true },
    age: { isNumeric: true, isTemporal: false, isCountable: true },
    createdAt: { isNumeric: false, isTemporal: true, isCountable: true },
    orders_total: { isNumeric: true, isTemporal: false, isCountable: true },
  },
  "users.orders": {
    id: { isNumeric: false, isTemporal: false, isCountable: true },
    total: { isNumeric: true, isTemporal: false, isCountable: true },
    createdAt: { isNumeric: false, isTemporal: true, isCountable: true },
  },
};

const ctx: AggregateValidatorContext = {
  baseTable: "users",
  joinOptions: [
    {
      name: "users.orders",
      table: "orders",
      path: [
        {
          source: ["users.id"],
          target: ["orders.userId"],
        },
      ],
      selectableColumns: ["id", "total", "createdAt"],
    },
  ],
  columnCatalog,
};

describe("aggregate validator", () => {
  it("accepts a valid base-table aggregate configuration", () => {
    const candidate = {
      joins: null,
      avg: ["users.age"],
      sum: ["users.orders_total"],
      min: ["users.createdAt"],
      max: ["users.createdAt"],
      count: ["users.id"],
      countDistinct: null,
      median: ["users.age"],
    };

    const result = validateAggregateCandidate(candidate, ctx);
    expect(result.status).toBe("valid");
    if (result.status === "valid") {
      expect(result.result.count).toEqual(["users.id"]);
      expect(result.result.avg).toEqual(["users.age"]);
      expect(result.result.joins).toBeNull();
    }
  });

  it("requires join alias when aggregating joined columns", () => {
    const candidate = {
      joins: [
        {
          table: "orders",
          name: "users.orders",
          path: [
            {
              source: ["users.id"],
              target: ["orders.userId"],
            },
          ],
        },
      ],
      avg: ["users.orders.total"],
      sum: null,
      min: null,
      max: null,
      count: null,
      countDistinct: null,
      median: null,
    };

    const result = validateAggregateCandidate(candidate, ctx);
    expect(result.status).toBe("valid");
    if (result.status === "valid") {
      expect(result.result.joins?.[0]?.name).toBe("users.orders");
      expect(result.result.avg).toEqual(["users.orders.total"]);
    }
  });

  it("raises an error when join alias is missing", () => {
    const candidate = {
      joins: [],
      avg: ["users.orders.total"],
      sum: null,
      min: null,
      max: null,
      count: null,
      countDistinct: null,
      median: null,
    };

    const result = validateAggregateCandidate(candidate, ctx);
    expect(result.status).toBe("invalid");
    if (result.status === "invalid") {
      expect(result.issues.some((issue) => issue.code === "missing_join")).toBe(
        true,
      );
    }
  });

  it("rejects non-numeric columns in numeric aggregates", () => {
    const candidate = {
      joins: null,
      avg: ["users.createdAt"],
      sum: null,
      min: null,
      max: null,
      count: null,
      countDistinct: null,
      median: null,
    };

    const result = validateAggregateCandidate(candidate, ctx);
    expect(result.status).toBe("invalid");
    if (result.status === "invalid") {
      expect(result.issues.some((issue) => issue.code === "non_numeric")).toBe(
        true,
      );
    }
  });

  it("rejects unknown columns", () => {
    const candidate = {
      joins: null,
      avg: ["users.unknown"],
      sum: null,
      min: null,
      max: null,
      count: null,
      countDistinct: null,
      median: null,
    };

    const result = validateAggregateCandidate(candidate, ctx);
    expect(result.status).toBe("invalid");
    if (result.status === "invalid") {
      expect(
        result.issues.some((issue) => issue.code === "invalid_column"),
      ).toBe(true);
    }
  });

  it("validates countDistinct aggregation", () => {
    const candidate = {
      joins: null,
      avg: null,
      sum: null,
      min: null,
      max: null,
      count: ["users.id"],
      countDistinct: ["users.age", "users.createdAt"],
      median: null,
    };

    const result = validateAggregateCandidate(candidate, ctx);
    expect(result.status).toBe("valid");
    if (result.status === "valid") {
      expect(result.result.countDistinct).toEqual([
        "users.age",
        "users.createdAt",
      ]);
      expect(result.result.count).toEqual(["users.id"]);
    }
  });
});
