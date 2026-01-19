import { describe, expect, it } from "vitest";
import {
  buildCountValidatorZodSchema,
  validateCountCandidate,
  type CountValidatorContext,
} from "./countValidator";

const ctx: CountValidatorContext = {
  baseTable: "users",
  baseColumns: ["id", "email", "countryId"],
  computedColumns: ["orders_total"],
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
};

describe("count validator", () => {
  it("valid candidate with joins passes", () => {
    const schema = buildCountValidatorZodSchema(ctx);
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
      count: ["_all", "users.orders.id"],
      countDistinct: ["users.email"],
    };
    expect(schema.safeParse(candidate).success).toBe(true);
    const result = validateCountCandidate(candidate, ctx);
    expect(result.status).toBe("valid");
    if (result.status === "valid") {
      expect(result.result.count).toEqual(["_all", "users.orders.id"]);
      expect(result.result.countDistinct).toEqual(["users.email"]);
      expect(result.result.joins?.[0]?.name).toBe("users.orders");
    }
  });

  it("invalid when join alias missing for column", () => {
    const candidate = {
      joins: [],
      count: ["users.orders.id"],
      countDistinct: null,
    };
    const result = validateCountCandidate(candidate, ctx);
    expect(result.status).toBe("invalid");
    if (result.status === "invalid") {
      expect(result.issues.some((issue) => issue.code === "missing_join")).toBe(
        true,
      );
    }
  });

  it("invalid when join path does not exist", () => {
    const candidate = {
      joins: [
        {
          table: "orders",
          name: "users.orders",
          path: [
            {
              source: ["users.email"],
              target: ["orders.invalid"],
            },
          ],
        },
      ],
      count: ["_all"],
      countDistinct: null,
    };
    const result = validateCountCandidate(candidate, ctx);
    expect(result.status).toBe("invalid");
    if (result.status === "invalid") {
      expect(
        result.issues.some((issue) => issue.code === "invalid_join_path"),
      ).toBe(true);
    }
  });

  it("invalid when column is not recognised", () => {
    const candidate = {
      joins: null,
      count: ["users.orders.unknown"],
      countDistinct: null,
    };
    const result = validateCountCandidate(candidate, ctx);
    expect(result.status).toBe("invalid");
    if (result.status === "invalid") {
      expect(
        result.issues.some(
          (issue) =>
            issue.code === "invalid_column" || issue.code === "invalid_value",
        ),
      ).toBe(true);
    }
  });

  it("rejects unqualified base columns", () => {
    const candidate = {
      joins: null,
      count: ["id"],
      countDistinct: null,
    };
    const result = validateCountCandidate(candidate, ctx);
    expect(result.status).toBe("invalid");
    if (result.status === "invalid") {
      expect(
        result.issues.some((issue) =>
          ["invalid_format", "invalid_value"].includes(issue.code),
        ),
      ).toBe(true);
    }
  });

  it("rejects duplicate aliases including the base table", () => {
    const candidate = {
      joins: [
        {
          table: "orders",
          name: "users",
          path: [
            {
              source: ["users.id"],
              target: ["orders.userId"],
            },
          ],
        },
      ],
      count: ["_all"],
      countDistinct: null,
    };
    const result = validateCountCandidate(candidate, ctx);
    expect(result.status).toBe("invalid");
    if (result.status === "invalid") {
      expect(
        result.issues.some((issue) => issue.code === "duplicate_join_alias"),
      ).toBe(true);
    }
  });

  it("validates distinct-only selections", () => {
    const candidate = {
      joins: null,
      count: null,
      countDistinct: ["users.email"],
    };
    const result = validateCountCandidate(candidate, ctx);
    expect(result.status).toBe("valid");
    if (result.status === "valid") {
      expect(result.result.count).toBeNull();
      expect(result.result.countDistinct).toEqual(["users.email"]);
    }
  });

  it("requires at least one metric", () => {
    const candidate = {
      joins: null,
      count: null,
      countDistinct: null,
    };
    const result = validateCountCandidate(candidate, ctx);
    expect(result.status).toBe("invalid");
    if (result.status === "invalid") {
      expect(
        result.issues.some(
          (issue) =>
            issue.path === "count" &&
            issue.message.includes("Provide at least one metric"),
        ),
      ).toBe(true);
      expect(
        result.issues.some(
          (issue) =>
            issue.path === "countDistinct" &&
            issue.message.includes("Provide at least one metric"),
        ),
      ).toBe(true);
    }
  });
});
