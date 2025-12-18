import { getColumnFromTable } from "./computedColumns";
import assert from "assert";
import type { SchemaResponse } from "~/types/types";

export function createAggregationFields<T>(
  columns: string[] | null | undefined,
  aggregationFn: (column: any) => any,
  schema: SchemaResponse
): [string, any][] | undefined {
  if (!columns || columns.length === 0) return undefined;
  return columns.map((columnIdentifier) => {
    assert(
      columnIdentifier.split(".").length === 2,
      "Invalid column format for aggregation (not table.column)"
    );
    const [tableName, columnName] = columnIdentifier.split(".");
    return [
      `${tableName}.${columnName}`,
      aggregationFn(
        getColumnFromTable({
          columnName,
          tableName,
          schema,
        })
      ),
    ];
  });
}
