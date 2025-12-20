import { describe, it, expect } from "vitest";
import {
  buildFindManyZodSchema,
  validateFindManyCandidate,
  type FindManyValidatorContext,
} from "./findManyValidator";

const ctx: FindManyValidatorContext = {
  baseTable: "users",
  selectableTableColumns: {
    users: ["id", "name", "createdAt"],
    "users.orders": ["id", "total", "createdAt"],
  },
  baseColumns: ["id", "name", "createdAt"],
  baseComputedColumns: ["_row_number"],
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
      selectableColumns: ["id", "total", "createdAt"],
    },
  ],
};

describe("findMany validator", () => {
  it("builds a schema that validates a correct candidate and strips empty arrays", () => {
    const schema = buildFindManyZodSchema(ctx);
    const candidate = {
      select: {
        users: ["id", "name"],
        "users.orders": [], // should be stripped
      },
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
      orderBy: { direction: "asc", column: "id" },
      limit: 25,
    };
    const parsed = schema.safeParse(candidate);
    expect(parsed.success).toBe(true);
    const result = validateFindManyCandidate(candidate, ctx);
    expect(result.status).toBe("valid");
    if (result.status === "valid") {
      expect(result.result.select).toEqual({ users: ["id", "name"] });
      expect(result.result.joins?.length).toBe(1);
      expect(result.result.limit).toBe(25);
    }
  });

  it("returns invalid with issues for bad orderBy column and limit", () => {
    const bad = {
      select: { users: ["id"] },
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
      select: { users: ["id"] },
      orderBy: null,
      limit: 5,
    };
    const result = validateFindManyCandidate(candidate, ctx);
    expect(result.status).toBe("valid");
  });
});
