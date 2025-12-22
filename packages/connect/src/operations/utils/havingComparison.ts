import { sql } from "kysely";
import type { Expression, SqlBool } from "kysely";
import assert from "assert";

const SUPPORTED_OPERATORS = new Set([
  "equals",
  "not",
  "in",
  "notIn",
  "lt",
  "lte",
  "gt",
  "gte",
]);

export function applyHavingComparison(
  expression: Expression<unknown>,
  operator: string,
  value: any,
): Expression<SqlBool> {
  assert(
    SUPPORTED_OPERATORS.has(operator),
    `Unsupported having operator ${operator}`,
  );

  switch (operator) {
    case "equals":
      if (value === null) return sql<SqlBool>`${expression} IS NULL`;
      return sql<SqlBool>`${expression} = ${value}`;
    case "not":
      if (value === null) return sql<SqlBool>`${expression} IS NOT NULL`;
      return sql<SqlBool>`${expression} != ${value}`;
    case "in":
      if (!Array.isArray(value) || value.length === 0) {
        return sql`FALSE`;
      }
      return sql<SqlBool>`${expression} IN (${sql.join(
        value.map((v) => sql`${v}`),
        sql`, `,
      )})`;
    case "notIn":
      if (!Array.isArray(value) || value.length === 0) {
        return sql`TRUE`;
      }
      return sql<SqlBool>`${expression} NOT IN (${sql.join(
        value.map((v) => sql`${v}`),
        sql`, `,
      )})`;
    case "lt":
      return sql<SqlBool>`${expression} < ${value}`;
    case "lte":
      return sql<SqlBool>`${expression} <= ${value}`;
    case "gt":
      return sql<SqlBool>`${expression} > ${value}`;
    case "gte":
      return sql<SqlBool>`${expression} >= ${value}`;
    default:
      assert(false, `Unsupported having operator ${operator}`);
  }
}
