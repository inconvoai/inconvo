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
  allPossibleColumns: [
    "users.id",
    "users.name",
    "orders.id",
    "orders.total",
    "profiles.id",
    "profiles.age",
  ],
  numericalColumns: ["orders.total", "profiles.age"],
};

describe("groupBy validator", () => {
  it("valid candidate passes and strips empty joins", () => {
    const schema = buildGroupByZodSchema(ctx);
    const candidate = {
      joins: [{ table: "orders", joinPath: "users>orders", joinType: "left" }],
      groupBy: ["users.id"],
      count: { columns: ["users.id"] },
      sum: { columns: ["orders.total"] },
      min: null,
      max: null,
      avg: null,
      orderBy: { function: "count", column: "users.id", direction: "asc" },
      limit: 10,
    };
    const parsed = schema.safeParse(candidate);
    expect(parsed.success).toBe(true);
    const result = validateGroupByCandidate(candidate, ctx);
    expect(result.status).toBe("valid");
    if (result.status === "valid") {
      expect(result.result.groupBy).toEqual(["users.id"]);
      expect(result.result.count?.columns).toEqual(["users.id"]);
    }
  });

  it("invalid when orderBy count column not in groupBy", () => {
    const bad = {
      joins: null,
      groupBy: ["users.id"],
      count: { columns: ["users.id"] },
      sum: null,
      min: null,
      max: null,
      avg: null,
      orderBy: { function: "count", column: "users.name", direction: "asc" },
      limit: 5,
    };
    const result = validateGroupByCandidate(bad, ctx);
    expect(result.status).toBe("invalid");
  });

  it("invalid when numeric function uses non-numeric column", () => {
    const bad = {
      joins: null,
      groupBy: ["users.id"],
      count: null,
      sum: { columns: ["users.name"] }, // not numeric
      min: null,
      max: null,
      avg: null,
      orderBy: { function: "sum", column: "users.name", direction: "asc" },
      limit: 5,
    };
    const result = validateGroupByCandidate(bad, ctx);
    expect(result.status).toBe("invalid");
  });
});
