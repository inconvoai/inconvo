import type { ValidateVirtualTableColumn } from "@repo/types";

const ISO_DATE_ONLY_REGEX = /^\d{4}-\d{2}-\d{2}$/u;
const ISO_DATETIME_REGEX =
  /^\d{4}-\d{2}-\d{2}[T ]\d{2}:\d{2}(?::\d{2}(?:\.\d{1,9})?)?(?:Z|[+-]\d{2}:\d{2})?$/u;

/**
 * Matches strings that are purely numeric (possibly negative, possibly decimal).
 * node-postgres returns bigint/numeric column values as JS strings to avoid
 * precision loss, e.g. COUNT(*) → "123", SUM(decimal_col) → "456.78".
 * We need to detect these and infer them as "number" instead of "string".
 */
const NUMERIC_STRING_REGEX = /^-?\d+(?:\.\d+)?$/u;

function looksLikeIsoDateOrDateTime(value: string): boolean {
  return ISO_DATE_ONLY_REGEX.test(value) || ISO_DATETIME_REGEX.test(value);
}

export function inferVirtualTableType(value: unknown): string {
  if (value === null || value === undefined) return "unknown";
  if (typeof value === "bigint") return "bigint";
  if (typeof value === "number")
    return Number.isInteger(value) ? "integer" : "number";
  if (typeof value === "boolean") return "boolean";
  if (value instanceof Date) return "DateTime";
  if (typeof value === "string") {
    // Check date-like strings first (they contain digits too)
    if (looksLikeIsoDateOrDateTime(value)) {
      const d = Date.parse(value);
      if (!Number.isNaN(d)) return "DateTime";
    }
    // Detect numeric strings from pg driver (bigint, numeric, decimal columns)
    if (NUMERIC_STRING_REGEX.test(value)) {
      return "number";
    }
    return "string";
  }
  if (Array.isArray(value)) return "json";
  if (typeof value === "object") return "json";
  return "unknown";
}

export function inferColumnsFromPreviewRows(
  rows: Record<string, unknown>[],
): ValidateVirtualTableColumn[] {
  const seen = new Set<string>();
  const orderedNames: string[] = [];
  for (const row of rows) {
    for (const key of Object.keys(row)) {
      if (!seen.has(key)) {
        seen.add(key);
        orderedNames.push(key);
      }
    }
  }

  return orderedNames.map((sourceName) => {
    let inferred: unknown = undefined;
    let sawNull = false;
    for (const row of rows) {
      const value = row[sourceName];
      if (value === null || value === undefined) {
        sawNull = true;
        continue;
      }
      inferred = value;
      break;
    }
    return {
      sourceName,
      type: inferVirtualTableType(inferred),
      nullable: sawNull ? true : inferred === undefined ? null : false,
    };
  });
}
