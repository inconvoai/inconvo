import { describe, test, expect } from "vitest";
import { stubSchema } from "./schemaValidator.stubSchema";
import { validateQuestionConditions } from "./dynamicSchema";

describe("questionWhere dynamic schema validator (stub schema)", () => {
  const fullSchema = stubSchema;
  const table = fullSchema[0];

  if (!table) {
    test.skip("Agent has no tables with selectable columns - skipping", () => {
      // Test skipped due to no available tables
    });
    return;
  }

  const stringCol = table.columns.find((c) => c.type === "string");
  const numberCol = table.columns.find((c) => c.type === "number");
  const booleanCol = table.columns.find((c) => c.type === "boolean");
  const dateCol = table.columns.find((c) => c.type === "DateTime");

  const toOneRel = table.outwardRelations.find((r) => r.isList === false);
  const toManyRel = table.outwardRelations.find((r) => r.isList === true);

  test("null (no filters) is valid", () => {
    const result = validateQuestionConditions(null, table, fullSchema);
    expect(result.success).toBe(true);
  });

  if (stringCol) {
    test("string column equality valid", () => {
      const candidate = {
        AND: [
          {
            [stringCol.name]: { equals: "Example Value" },
          },
        ],
      };
      const result = validateQuestionConditions(candidate, table, fullSchema);
      expect(result.success).toBe(true);
    });
  }

  if (numberCol) {
    test("number column comparison valid", () => {
      const candidate = {
        AND: [
          {
            [numberCol.name]: { gt: 10 },
          },
        ],
      };
      const result = validateQuestionConditions(candidate, table, fullSchema);
      expect(result.success).toBe(true);
    });
  }

  if (booleanCol) {
    test("boolean column equals valid", () => {
      const candidate = {
        AND: [
          {
            [booleanCol.name]: { equals: true },
          },
        ],
      };
      const result = validateQuestionConditions(candidate, table, fullSchema);
      expect(result.success).toBe(true);
    });
  }

  if (dateCol) {
    test("date column gte valid", () => {
      const candidate = {
        AND: [
          {
            [dateCol.name]: { gte: new Date().toISOString() },
          },
        ],
      };
      const result = validateQuestionConditions(candidate, table, fullSchema);
      expect(result.success).toBe(true);
    });
  }

  if (toOneRel) {
    const targetTable = fullSchema.find(
      (t) => t.name === toOneRel.targetTable?.name,
    );
    const targetString = targetTable?.columns.find((c) => c.type === "string");
    if (targetString) {
      test("to-one relation 'is' single column condition valid", () => {
        const candidate = {
          AND: [
            {
              [toOneRel.name]: {
                is: { [targetString.name]: { contains: "abc" } },
              },
            },
          ],
        };
        const result = validateQuestionConditions(candidate, table, fullSchema);
        expect(result.success).toBe(true);
      });
    }
  }

  if (toManyRel) {
    const targetTable = fullSchema.find(
      (t) => t.name === toManyRel.targetTable?.name,
    );
    const targetNumber = targetTable?.columns.find((c) => c.type === "number");
    if (targetNumber) {
      test("to-many relation 'some' numeric condition valid", () => {
        const candidate = {
          AND: [
            {
              [toManyRel.name]: {
                some: { [targetNumber.name]: { lt: 100 } },
              },
            },
          ],
        };
        const result = validateQuestionConditions(candidate, table, fullSchema);
        expect(result.success).toBe(true);
      });
    }
  }

  test("unknown column invalid", () => {
    const candidate = {
      AND: [
        {
          __DefinitelyNotARealColumn__: { equals: 1 },
        },
      ],
    };
    const result = validateQuestionConditions(candidate, table, fullSchema);
    expect(result.success).toBe(false);
  });

  if (stringCol) {
    test("unsupported operator invalid", () => {
      const candidate = {
        AND: [
          {
            [stringCol.name]: { startsWith: "abc" }, // startsWith not in dynamic schema
          },
        ],
      };
      const result = validateQuestionConditions(candidate, table, fullSchema);
      expect(result.success).toBe(false);
    });
  }

  if (stringCol && numberCol) {
    test("multiple columns in single filter object invalid", () => {
      const candidate = {
        AND: [
          {
            [stringCol.name]: { equals: "x" },
            [numberCol.name]: { equals: 5 },
          },
        ],
      };
      const result = validateQuestionConditions(candidate, table, fullSchema);
      expect(result.success).toBe(false);
    });
  }

  // Additional invalid cases involving non-existent columns / relations

  if (toOneRel) {
    const targetTable = fullSchema.find(
      (t) => t.name === toOneRel.targetTable?.name,
    );
    // Pick a column name that is definitely not present in the target table
    const bogusInnerCol = "__NoSuchInnerColumn__";
    const innerHasCol = targetTable?.columns.some(
      (c) => c.name === bogusInnerCol,
    );
    if (!innerHasCol) {
      test("to-one relation with non-existent inner column invalid", () => {
        const candidate = {
          AND: [
            {
              [toOneRel.name]: {
                is: { [bogusInnerCol]: { equals: "whatever" } },
              },
            },
          ],
        };
        const result = validateQuestionConditions(candidate, table, fullSchema);
        expect(result.success).toBe(false);
      });
    }
  }

  if (toManyRel) {
    const targetTable = fullSchema.find(
      (t) => t.name === toManyRel.targetTable?.name,
    );
    const bogusInnerCol = "__NoSuchInnerColumn__";
    const innerHasCol = targetTable?.columns.some(
      (c) => c.name === bogusInnerCol,
    );
    if (!innerHasCol) {
      test("to-many relation with non-existent inner column invalid", () => {
        const candidate = {
          AND: [
            {
              [toManyRel.name]: {
                some: { [bogusInnerCol]: { equals: 123 } },
              },
            },
          ],
        };
        const result = validateQuestionConditions(candidate, table, fullSchema);
        expect(result.success).toBe(false);
      });
    }
  }

  test("non-existent relation name invalid", () => {
    const candidate = {
      AND: [
        {
          __FakeRelation__: {
            is: { __InnerFakeCol__: { equals: 1 } },
          },
        },
      ],
    };
    const result = validateQuestionConditions(candidate, table, fullSchema);
    expect(result.success).toBe(false);
  });

  test("nested logical group with unknown column invalid", () => {
    const candidate = {
      AND: [
        {
          AND: [
            {
              __DeepUnknown__: { equals: "x" },
            },
          ],
        },
      ],
    };
    const result = validateQuestionConditions(candidate, table, fullSchema);
    expect(result.success).toBe(false);
  });

  if (stringCol) {
    test("valid AND group mixed with invalid unknown column fails", () => {
      const candidate = {
        AND: [
          { [stringCol.name]: { equals: "X" } },
          { __UnknownCol__: { equals: 5 } },
        ],
      };
      const result = validateQuestionConditions(candidate, table, fullSchema);
      expect(result.success).toBe(false);
    });
  }
});
