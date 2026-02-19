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

  const tableName = table.name;

  // Helper to qualify column names
  const qualify = (colName: string) => `${tableName}.${colName}`;

  const stringCol = table.columns.find((c) => c.type === "string");
  const numberCol = table.columns.find((c) => c.type === "number");
  const booleanCol = table.columns.find((c) => c.type === "boolean");
  const dateCol = table.columns.find((c) => c.type === "DateTime");

  const toOneRel = table.outwardRelations.find((r) => r.isList === false);
  const toManyRel = table.outwardRelations.find((r) => r.isList === true);

  const cloneSchema = () => JSON.parse(JSON.stringify(fullSchema)) as typeof fullSchema;

  test("null (no filters) is valid", () => {
    const result = validateQuestionConditions(null, table, fullSchema, tableName);
    expect(result.success).toBe(true);
  });

  if (stringCol) {
    test("string column equality valid", () => {
      const candidate = {
        AND: [
          {
            [qualify(stringCol.name)]: { equals: "Example Value" },
          },
        ],
      };
      const result = validateQuestionConditions(candidate, table, fullSchema, tableName);
      expect(result.success).toBe(true);
    });
  }

  if (numberCol) {
    test("number column comparison valid", () => {
      const candidate = {
        AND: [
          {
            [qualify(numberCol.name)]: { gt: 10 },
          },
        ],
      };
      const result = validateQuestionConditions(candidate, table, fullSchema, tableName);
      expect(result.success).toBe(true);
    });
  }

  if (booleanCol) {
    test("boolean column equals valid", () => {
      const candidate = {
        AND: [
          {
            [qualify(booleanCol.name)]: { equals: true },
          },
        ],
      };
      const result = validateQuestionConditions(candidate, table, fullSchema, tableName);
      expect(result.success).toBe(true);
    });
  }

  if (dateCol) {
    test("date column gte valid", () => {
      const candidate = {
        AND: [
          {
            [qualify(dateCol.name)]: { gte: new Date().toISOString() },
          },
        ],
      };
      const result = validateQuestionConditions(candidate, table, fullSchema, tableName);
      expect(result.success).toBe(true);
    });
  }

  if (numberCol) {
    test("numeric enum resolves label to canonical numeric value", () => {
      const enumSchema = cloneSchema();
      const enumTable = enumSchema.find((t) => t.name === tableName);
      expect(enumTable).toBeTruthy();
      if (!enumTable) return;

      const targetColumn = enumTable.columns.find((c) => c.name === numberCol.name);
      expect(targetColumn).toBeTruthy();
      if (!targetColumn) return;

      (targetColumn as { valueEnum?: unknown }).valueEnum = {
        id: "enum_status",
        selected: true,
        entries: [
          { value: 0, label: "Plan Used", selected: true, position: 0 },
          { value: 1, label: "Joined", selected: true, position: 1 },
        ],
      };

      const candidate = {
        AND: [
          {
            [qualify(numberCol.name)]: { equals: "Joined" },
          },
        ],
      };
      const result = validateQuestionConditions(candidate, enumTable, enumSchema, tableName);
      expect(result.success).toBe(true);
      if (result.success) {
        expect(
          (result.data as { AND: Array<Record<string, { equals: number }>> }).AND[0]?.[
            qualify(numberCol.name)
          ]?.equals,
        ).toBe(1);
      }
    });

    test("numeric enum rejects range operators", () => {
      const enumSchema = cloneSchema();
      const enumTable = enumSchema.find((t) => t.name === tableName);
      expect(enumTable).toBeTruthy();
      if (!enumTable) return;

      const targetColumn = enumTable.columns.find((c) => c.name === numberCol.name);
      expect(targetColumn).toBeTruthy();
      if (!targetColumn) return;

      (targetColumn as { valueEnum?: unknown }).valueEnum = {
        id: "enum_status",
        selected: true,
        entries: [
          { value: 0, label: "Plan Used", selected: true, position: 0 },
          { value: 1, label: "Joined", selected: true, position: 1 },
        ],
      };

      const candidate = {
        AND: [
          {
            [qualify(numberCol.name)]: { gt: 0 },
          },
        ],
      };
      const result = validateQuestionConditions(candidate, enumTable, enumSchema, tableName);
      expect(result.success).toBe(false);
    });
  }

  if (stringCol) {
    test("string enum resolves label to canonical value and rejects unknown labels", () => {
      const enumSchema = cloneSchema();
      const enumTable = enumSchema.find((t) => t.name === tableName);
      expect(enumTable).toBeTruthy();
      if (!enumTable) return;

      const targetColumn = enumTable.columns.find((c) => c.name === stringCol.name);
      expect(targetColumn).toBeTruthy();
      if (!targetColumn) return;

      (targetColumn as { valueEnum?: unknown }).valueEnum = {
        id: "enum_string",
        selected: true,
        entries: [
          { value: "alpha", label: "Alpha Friendly", selected: true, position: 0 },
          { value: "beta", label: "Beta Friendly", selected: true, position: 1 },
        ],
      };

      const allowedCandidate = {
        AND: [
          {
            [qualify(stringCol.name)]: { equals: "Alpha Friendly" },
          },
        ],
      };
      const deniedCandidate = {
        AND: [
          {
            [qualify(stringCol.name)]: { equals: "Gamma Friendly" },
          },
        ],
      };

      const allowedResult = validateQuestionConditions(
        allowedCandidate,
        enumTable,
        enumSchema,
        tableName,
      );
      const deniedResult = validateQuestionConditions(
        deniedCandidate,
        enumTable,
        enumSchema,
        tableName,
      );

      expect(allowedResult.success).toBe(true);
      if (allowedResult.success) {
        expect(
          (allowedResult.data as { AND: Array<Record<string, { equals: string }>> })
            .AND[0]?.[qualify(stringCol.name)]?.equals,
        ).toBe("alpha");
      }
      expect(deniedResult.success).toBe(false);
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
        const result = validateQuestionConditions(candidate, table, fullSchema, tableName);
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
        const result = validateQuestionConditions(candidate, table, fullSchema, tableName);
        expect(result.success).toBe(true);
      });
    }
  }

  // ── Multi-operator condition tests ──────────────────────────────

  if (dateCol) {
    test("date column with combined gte + lte (range) is valid", () => {
      const candidate = {
        AND: [
          {
            [qualify(dateCol.name)]: {
              gte: "2025-01-01T00:00:00.000Z",
              lte: "2025-12-31T23:59:59.999Z",
            },
          },
        ],
      };
      const result = validateQuestionConditions(candidate, table, fullSchema, tableName);
      expect(result.success).toBe(true);
    });
  }

  if (numberCol) {
    test("number column with combined gte + lte (range) is valid", () => {
      const candidate = {
        AND: [
          {
            [qualify(numberCol.name)]: { gte: 10, lte: 100 },
          },
        ],
      };
      const result = validateQuestionConditions(candidate, table, fullSchema, tableName);
      expect(result.success).toBe(true);
    });

    test("number column with combined gt + lt (exclusive range) is valid", () => {
      const candidate = {
        AND: [
          {
            [qualify(numberCol.name)]: { gt: 0, lt: 1000 },
          },
        ],
      };
      const result = validateQuestionConditions(candidate, table, fullSchema, tableName);
      expect(result.success).toBe(true);
    });
  }

  if (toManyRel) {
    const targetTable = fullSchema.find(
      (t) => t.name === toManyRel.targetTable?.name,
    );
    const targetDate = targetTable?.columns.find((c) => c.type === "DateTime");
    if (targetDate) {
      test("to-many relation 'some' with multi-operator date range is valid", () => {
        const candidate = {
          AND: [
            {
              [toManyRel.name]: {
                some: {
                  [targetDate.name]: {
                    gte: "2025-01-01T00:00:00.000Z",
                    lte: "2025-12-31T23:59:59.999Z",
                  },
                },
              },
            },
          ],
        };
        const result = validateQuestionConditions(candidate, table, fullSchema, tableName);
        expect(result.success).toBe(true);
      });
    }
  }

  if (stringCol) {
    test("string column with multiple operators is invalid", () => {
      const candidate = {
        AND: [
          {
            [qualify(stringCol.name)]: { equals: "x", contains: "y" },
          },
        ],
      };
      const result = validateQuestionConditions(candidate, table, fullSchema, tableName);
      expect(result.success).toBe(false);
    });
  }

  if (booleanCol) {
    test("boolean column with multiple operators is invalid", () => {
      const candidate = {
        AND: [
          {
            [qualify(booleanCol.name)]: { equals: true, not: false },
          },
        ],
      };
      const result = validateQuestionConditions(candidate, table, fullSchema, tableName);
      expect(result.success).toBe(false);
    });
  }

  // ── End multi-operator tests ──────────────────────────────────

  test("unknown column invalid", () => {
    const candidate = {
      AND: [
        {
          __DefinitelyNotARealColumn__: { equals: 1 },
        },
      ],
    };
    const result = validateQuestionConditions(candidate, table, fullSchema, tableName);
    expect(result.success).toBe(false);
  });

  if (stringCol) {
    test("unsupported operator invalid", () => {
      const candidate = {
        AND: [
          {
            [qualify(stringCol.name)]: { startsWith: "abc" }, // startsWith not in dynamic schema
          },
        ],
      };
      const result = validateQuestionConditions(candidate, table, fullSchema, tableName);
      expect(result.success).toBe(false);
    });
  }

  if (stringCol && numberCol) {
    test("multiple columns in single filter object invalid", () => {
      const candidate = {
        AND: [
          {
            [qualify(stringCol.name)]: { equals: "x" },
            [qualify(numberCol.name)]: { equals: 5 },
          },
        ],
      };
      const result = validateQuestionConditions(candidate, table, fullSchema, tableName);
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
        const result = validateQuestionConditions(candidate, table, fullSchema, tableName);
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
        const result = validateQuestionConditions(candidate, table, fullSchema, tableName);
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
    const result = validateQuestionConditions(candidate, table, fullSchema, tableName);
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
    const result = validateQuestionConditions(candidate, table, fullSchema, tableName);
    expect(result.success).toBe(false);
  });

  if (stringCol) {
    test("valid AND group mixed with invalid unknown column fails", () => {
      const candidate = {
        AND: [
          { [qualify(stringCol.name)]: { equals: "X" } },
          { __UnknownCol__: { equals: 5 } },
        ],
      };
      const result = validateQuestionConditions(candidate, table, fullSchema, tableName);
      expect(result.success).toBe(false);
    });
  }
});
