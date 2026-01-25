import { getColumnFromTable } from "./computedColumns";
import assert from "assert";
import type { SchemaResponse } from "../../types/types";
import type { DatabaseDialect } from "../types";

export function createAggregationFields(
  columns: string[] | null | undefined,
  aggregationFn: (column: any) => any,
  schema: SchemaResponse,
  dialect: DatabaseDialect,
): [string, any][] | undefined {
  if (!columns || columns.length === 0) return undefined;
  return columns.map((columnIdentifier) => {
    assert(
      columnIdentifier.split(".").length === 2,
      "Invalid column format for aggregation (not table.column)",
    );
    const [tableName, columnName] = columnIdentifier.split(".") as [
      string,
      string,
    ];
    return [
      `${tableName}.${columnName}`,
      aggregationFn(
        getColumnFromTable({
          columnName,
          tableName,
          schema,
          dialect,
        }),
      ),
    ];
  });
}
