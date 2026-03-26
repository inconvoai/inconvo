import assert from "assert";
import { sql, type Expression } from "kysely";
import { getColumnFromTable } from "./computedColumns";
import type { SchemaResponse } from "../../types/types";
import type { DatabaseDialect } from "../types";

export type AggregateFn =
  | "count"
  | "countDistinct"
  | "sum"
  | "min"
  | "max"
  | "avg";

export function buildAggregateExpression(
  fn: AggregateFn,
  columnRef: string,
  schema: SchemaResponse,
  dialect: DatabaseDialect,
  aliasToTable?: Map<string, string>,
): Expression<unknown> {
  const lastDot = columnRef.lastIndexOf(".");
  assert(lastDot !== -1, "Aggregate column must be in the format table.column");
  const tableOrAlias = columnRef.slice(0, lastDot);
  const columnName = columnRef.slice(lastDot + 1);
  const tableName = aliasToTable?.get(tableOrAlias) ?? tableOrAlias;
  const column = getColumnFromTable({
    columnName,
    tableName,
    schema,
    dialect,
  });

  switch (fn) {
    case "count":
      return sql`COUNT(${column})`;
    case "countDistinct":
      return sql`COUNT(DISTINCT ${column})`;
    case "sum":
      return sql`SUM(${column})`;
    case "min":
      return sql`MIN(${column})`;
    case "max":
      return sql`MAX(${column})`;
    case "avg":
      return sql`AVG(${column})`;
    default:
      assert(false, `Unsupported aggregate function ${fn}`);
  }
}
