import { describe, expect, it } from "vitest";
import {
  validateFindDistinctCandidate,
  type FindDistinctValidatorContext,
} from "./findDistinctValidator";

const ctx: FindDistinctValidatorContext = {
  baseTable: "users",
  selectableColumns: ["users.id", "users.email", "users.countryId"],
};

describe("findDistinct validator", () => {
  it("accepts a valid fully-qualified column", () => {
    const result = validateFindDistinctCandidate(
      { column: "users.email" },
      ctx,
    );
    expect(result.status).toBe("valid");
    if (result.status === "valid") {
      expect(result.result.column).toBe("users.email");
    }
  });

  it("rejects columns that are not fully qualified", () => {
    const result = validateFindDistinctCandidate({ column: "email" }, ctx);
    expect(result.status).toBe("invalid");
    if (result.status === "invalid") {
      expect(
        result.issues.some((issue) => issue.code === "invalid_format"),
      ).toBe(true);
    }
  });

  it("rejects columns from other tables", () => {
    const result = validateFindDistinctCandidate({ column: "orders.id" }, ctx);
    expect(result.status).toBe("invalid");
    if (result.status === "invalid") {
      expect(
        result.issues.some((issue) => issue.code === "invalid_table"),
      ).toBe(true);
    }
  });

  it("rejects unknown columns", () => {
    const result = validateFindDistinctCandidate(
      { column: "users.unknown" },
      ctx,
    );
    expect(result.status).toBe("invalid");
    if (result.status === "invalid") {
      expect(
        result.issues.some((issue) => issue.code === "invalid_column"),
      ).toBe(true);
    }
  });
});
