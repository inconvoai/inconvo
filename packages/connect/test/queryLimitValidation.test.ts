import { QuerySchema } from "../src/types/querySchema";
import { applyLimit } from "../src/operations/utils/queryHelpers";

describe("query limit validation", () => {
  test("applyLimit uses limit(0) for non-MSSQL instead of treating it as unset", () => {
    const limit = jest.fn().mockReturnValue("limited");
    const top = jest.fn().mockReturnValue("topped");
    const query = { limit, top } as any;

    const result = applyLimit(query, 0, "postgresql");

    expect(result).toBe("limited");
    expect(limit).toHaveBeenCalledWith(0);
    expect(top).not.toHaveBeenCalled();
  });

  test("applyLimit uses top(0) for MSSQL instead of treating it as unset", () => {
    const limit = jest.fn().mockReturnValue("limited");
    const top = jest.fn().mockReturnValue("topped");
    const query = { limit, top } as any;

    const result = applyLimit(query, 0, "mssql");

    expect(result).toBe("topped");
    expect(top).toHaveBeenCalledWith(0);
    expect(limit).not.toHaveBeenCalled();
  });

  test("rejects limit=0 for findMany", () => {
    expect(() =>
      QuerySchema.parse({
        table: "users",
        tableConditions: null,
        whereAndArray: [],
        operation: "findMany" as const,
        operationParameters: {
          select: { users: ["id"] },
          joins: null,
          orderBy: null,
          limit: 0,
        },
      }),
    ).toThrow();
  });

  test("rejects limit=0 for countRelations", () => {
    expect(() =>
      QuerySchema.parse({
        table: "users",
        tableConditions: null,
        whereAndArray: [],
        operation: "countRelations" as const,
        operationParameters: {
          columns: ["id"],
          joins: null,
          relationsToCount: [{ name: "orders", distinct: null }],
          orderBy: null,
          limit: 0,
        },
      }),
    ).toThrow();
  });

  test("rejects limit=0 for groupBy", () => {
    expect(() =>
      QuerySchema.parse({
        table: "users",
        tableConditions: null,
        whereAndArray: [],
        operation: "groupBy" as const,
        operationParameters: {
          joins: null,
          groupBy: [{ type: "column", column: "users.id" }],
          count: ["users.id"],
          countDistinct: null,
          sum: null,
          min: null,
          max: null,
          avg: null,
          orderBy: {
            type: "aggregate" as const,
            function: "count" as const,
            column: "users.id",
            direction: "desc" as const,
          },
          limit: 0,
          having: null,
        },
      }),
    ).toThrow();
  });
});
