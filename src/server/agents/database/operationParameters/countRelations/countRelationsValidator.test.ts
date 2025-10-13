import { describe, it, expect } from "vitest";
import {
  buildCountRelationsZodSchema,
  validateCountRelationsCandidate,
  type CountRelationsValidatorContext,
} from "./countRelationsValidator";

const ctx: CountRelationsValidatorContext = {
  baseColumns: ["id", "name"],
  relationOptions: [
    { name: "orders", targetColumns: ["id", "total", "userId"] },
    { name: "sessions", targetColumns: ["id", "duration"] },
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
      orderBy: { relation: "orders", direction: "desc" },
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
      orderBy: { relation: "sessions", direction: "asc" }, // sessions not in relationsToCount
      limit: 5,
    };
    const result = validateCountRelationsCandidate(candidate, ctx);
    expect(result.status).toBe("invalid");
  });

  it("invalid empty columns", () => {
    const candidate = {
      columns: [],
      relationsToCount: [{ name: "orders", distinct: null }],
      orderBy: null,
      limit: 10,
    };
    const result = validateCountRelationsCandidate(candidate, ctx);
    expect(result.status).toBe("invalid");
  });
});
