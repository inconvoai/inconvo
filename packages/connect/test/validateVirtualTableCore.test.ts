// @ts-nocheck
import type { ValidateVirtualTableColumn } from "@repo/types";
import type { DatabaseDialect } from "~/operations/types";
import {
  classifyVirtualTableInferenceError,
  type InferColumnsFromDbResult,
  type ValidateVirtualTableCoreDeps,
  validateVirtualTableCore,
} from "~/util/validateVirtualTableCore";

function makeDeps(
  previewRows: Record<string, unknown>[],
  inferResult: InferColumnsFromDbResult,
): ValidateVirtualTableCoreDeps {
  return {
    executeProbeQuery: async () => ({ rows: previewRows }),
    inferColumnsFromDb: async () => inferResult,
  };
}

function makeInput(dialect: DatabaseDialect = "postgresql") {
  return {
    sql: "SELECT 1 AS id",
    dialect,
    previewLimit: 5,
    db: {} as any,
  };
}

describe("classifyVirtualTableInferenceError", () => {
  test("classifies ambiguous-column SQLSTATE errors", () => {
    const classified = classifyVirtualTableInferenceError({
      code: "42702",
      message: 'column reference "id" is ambiguous',
    });

    expect(classified).toEqual({
      kind: "ambiguous_column_reference",
      columnName: "id",
    });
  });

  test("classifies ambiguous-column errors by message fallback", () => {
    const classified = classifyVirtualTableInferenceError({
      message: "ERROR: column reference id is ambiguous",
    });

    expect(classified).toEqual({
      kind: "ambiguous_column_reference",
      columnName: "id",
    });
  });

  test("returns null for non-ambiguous errors", () => {
    const classified = classifyVirtualTableInferenceError({
      code: "22007",
      message: "invalid datetime format",
    });

    expect(classified).toBeNull();
  });
});

describe("validateVirtualTableCore", () => {
  test("returns alias guidance for ambiguous join columns", async () => {
    const response = await validateVirtualTableCore(
      makeInput("postgresql"),
      makeDeps([{ id: 1 }], {
        kind: "error",
        error: {
          kind: "ambiguous_column_reference",
          columnName: "id",
        },
      }),
    );

    expect(response.ok).toBe(false);
    expect(response.error?.message).toContain('Ambiguous column reference detected for "id"');
    expect(response.error?.message).toContain("Use explicit column aliases");
  });

  test("returns metadata inference failure when inferred columns are unavailable", async () => {
    const response = await validateVirtualTableCore(
      makeInput("redshift"),
      makeDeps([], { kind: "columns", columns: null }),
    );

    expect(response).toEqual({
      ok: false,
      error: {
        message:
          "Could not infer virtual-table column types from redshift metadata. This can happen when the validation preview returns zero rows (no column names are available for pg_typeof). Try validating with a query/filter that returns at least one row.",
      },
    });
  });

  test("returns inferred columns and preview rows when inference succeeds", async () => {
    const columns: ValidateVirtualTableColumn[] = [
      { sourceName: "id", type: "number", nullable: false },
    ];
    const previewRows = [{ id: 1 }];

    const response = await validateVirtualTableCore(
      makeInput("postgresql"),
      makeDeps(previewRows, { kind: "columns", columns }),
    );

    expect(response).toEqual({
      ok: true,
      columns,
      previewRows,
    });
  });
});
