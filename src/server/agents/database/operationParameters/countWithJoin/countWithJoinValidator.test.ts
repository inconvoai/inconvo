import { describe, it, expect } from "vitest";
import {
  buildCountWithJoinZodSchema,
  validateCountWithJoinCandidate,
} from "./countWithJoinValidator";

const ctx = {
  baseTable: "users",
  joinOptions: [{ table: "orders", joinPath: "users>orders" }],
  allPossibleColumns: ["users.id", "users.name", "orders.id", "orders.total"],
};

describe("countWithJoin validator", () => {
  it("valid candidate passes", () => {
    const schema = buildCountWithJoinZodSchema(ctx);
    const candidateOperationParameters = {
      joins: [{ table: "orders", joinPath: "users>orders", joinType: "inner" }],
      count: ["users.id", "orders.id"],
      countDistinct: ["orders.id"],
    };
    const parsed = schema.safeParse(candidateOperationParameters);
    expect(parsed.success).toBe(true);
    const result = validateCountWithJoinCandidate(
      candidateOperationParameters,
      ctx
    );
    expect(result.status).toBe("valid");
  });

  it("invalid if referencing unjoined table column", () => {
    const candidateOperationParameters = {
      joins: [{ table: "orders", joinPath: "users>orders", joinType: "left" }],
      count: ["users.id", "profiles.id"], // profiles not joined
      countDistinct: null,
    };
    const result = validateCountWithJoinCandidate(
      candidateOperationParameters,
      ctx
    );
    expect(result.status).toBe("invalid");
  });

  it("invalid if no joins provided", () => {
    const candidateOperationParameters = {
      joins: [],
      count: ["users.id"],
      countDistinct: null,
    };
    const result = validateCountWithJoinCandidate(
      candidateOperationParameters,
      ctx
    );
    expect(result.status).toBe("invalid");
  });
});
