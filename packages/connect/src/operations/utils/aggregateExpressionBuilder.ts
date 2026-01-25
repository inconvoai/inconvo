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
): Expression<unknown> {
  assert(
    columnRef.split(".").length === 2,
    "Aggregate column must be in the format table.column",
  );
  const [tableName, columnName] = columnRef.split(".") as [string, string];
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
