import { describe, it, expect } from "vitest";
import {
  buildFindManyZodSchema,
  validateFindManyCandidate,
} from "./findManyValidator";

const ctx = {
  selectableTableColumns: {
    base: ["id", "name", "createdAt"],
    "base>orders": ["id", "total", "createdAt"],
  },
  baseColumns: ["id", "name", "createdAt"],
  baseComputedColumns: ["_row_number"],
};

describe("findMany validator", () => {
  it("builds a schema that validates a correct candidate and strips empty arrays", () => {
    const schema = buildFindManyZodSchema(ctx);
    const candidate = {
      columns: {
        base: ["id", "name"],
        "base>orders": [], // should be stripped
      },
      orderBy: { direction: "asc", column: "id" },
      limit: 25,
    };
    const parsed = schema.safeParse(candidate);
    expect(parsed.success).toBe(true);
    const result = validateFindManyCandidate(candidate, ctx);
    expect(result.status).toBe("valid");
    if (result.status === "valid") {
      expect(result.result.columns).toEqual({ base: ["id", "name"] });
      expect(result.result.limit).toBe(25);
    }
  });

  it("returns invalid with issues for bad orderBy column and limit", () => {
    const bad = {
      columns: { base: ["id"] },
      orderBy: { direction: "asc", column: "nonexistent" },
      limit: 50000,
    };
    const result = validateFindManyCandidate(bad, ctx);
    expect(result.status).toBe("invalid");
    if (result.status === "invalid") {
      // Should flag at least 2 issues (bad column + limit too large)
      expect(result.issues.length).toBeGreaterThanOrEqual(2);
    }
  });

  it("allows null orderBy", () => {
    const candidate = {
      columns: { base: ["id"] },
      orderBy: null,
      limit: 5,
    };
    const result = validateFindManyCandidate(candidate, ctx);
    expect(result.status).toBe("valid");
  });
});
