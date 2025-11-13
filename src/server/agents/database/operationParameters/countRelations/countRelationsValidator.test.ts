import { describe, it, expect } from "vitest";
import {
  buildCountRelationsZodSchema,
  validateCountRelationsCandidate,
  type CountRelationsValidatorContext,
} from "./countRelationsValidator";

const ctx: CountRelationsValidatorContext = {
  baseColumns: ["id", "name"],
  relationOptions: [
    {
      name: "orders",
      table: "orders",
      path: [
        {
          source: ["users.id"],
          target: ["orders.userId"],
        },
      ],
      targetColumns: ["id", "total", "userId"],
    },
    {
      name: "sessions",
      table: "sessions",
      path: [
        {
          source: ["users.id"],
          target: ["sessions.userId"],
        },
      ],
      targetColumns: ["id", "duration"],
    },
  ],
};

describe("countRelations validator", () => {
  it("valid candidate passes", () => {
    const schema = buildCountRelationsZodSchema(ctx);
    const candidate = {
      columns: ["id"],
      relationsToCount: [
        { name: "orders", distinct: "id" },
        { name: "sessions", distinct: null },
      ],
      joins: [
        {
          table: "orders",
          name: "orders",
          path: [
            {
              source: ["users.id"],
              target: ["orders.userId"],
            },
          ],
        },
        {
          table: "sessions",
          name: "sessions",
          path: [
            {
              source: ["users.id"],
              target: ["sessions.userId"],
            },
          ],
        },
      ],
      orderBy: { name: "orders", direction: "desc" },
      limit: 25,
    };
    const parsed = schema.safeParse(candidate);
    expect(parsed.success).toBe(true);
    const result = validateCountRelationsCandidate(candidate, ctx);
    expect(result.status).toBe("valid");
  });

  it("invalid if duplicate relations", () => {
    const candidate = {
      columns: ["id"],
      relationsToCount: [
        { name: "orders", distinct: null },
        { name: "orders", distinct: null },
      ],
      joins: [
        {
          table: "orders",
          name: "orders",
          path: [
            {
              source: ["users.id"],
              target: ["orders.userId"],
            },
          ],
        },
      ],
      orderBy: null,
      limit: 10,
    };
    const result = validateCountRelationsCandidate(candidate, ctx);
    expect(result.status).toBe("invalid");
  });

  it("invalid if distinct column not in relation", () => {
    const candidate = {
      columns: ["id"],
      relationsToCount: [{ name: "orders", distinct: "duration" }], // duration not in orders
      joins: [
        {
          table: "orders",
          name: "orders",
          path: [
            {
              source: ["users.id"],
              target: ["orders.userId"],
            },
          ],
        },
      ],
      orderBy: null,
      limit: 10,
    };
    const result = validateCountRelationsCandidate(candidate, ctx);
    expect(result.status).toBe("invalid");
  });

  it("invalid orderBy relation not counted", () => {
    const candidate = {
      columns: ["id"],
      relationsToCount: [{ name: "orders", distinct: null }],
      joins: [
        {
          table: "orders",
          name: "orders",
          path: [
            {
              source: ["users.id"],
              target: ["orders.userId"],
            },
          ],
        },
      ],
      orderBy: { name: "sessions", direction: "asc" }, // sessions not in relationsToCount
      limit: 5,
    };
    const result = validateCountRelationsCandidate(candidate, ctx);
    expect(result.status).toBe("invalid");
  });

  it("invalid empty columns", () => {
    const candidate = {
      columns: [],
      relationsToCount: [{ name: "orders", distinct: null }],
      joins: [
        {
          table: "orders",
          name: "orders",
          path: [
            {
              source: ["users.id"],
              target: ["orders.userId"],
            },
          ],
        },
      ],
      orderBy: null,
      limit: 10,
    };
    const result = validateCountRelationsCandidate(candidate, ctx);
    expect(result.status).toBe("invalid");
  });

  it("invalid when required join is missing", () => {
    const candidate = {
      columns: ["id"],
      relationsToCount: [{ name: "orders", distinct: null }],
      joins: [],
      orderBy: null,
      limit: 10,
    };
    const result = validateCountRelationsCandidate(candidate, ctx);
    expect(result.status).toBe("invalid");
    if (result.status === "invalid") {
      expect(
        result.issues.some(
          (issue) => issue.code === "missing_join_descriptor"
        )
      ).toBe(true);
    }
  });
});
