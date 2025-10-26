import { SQL } from "drizzle-orm";
import { getColumnFromTable } from "./getColumnFromTable";
import assert from "assert";
import { ComputedColumn } from "~/types/querySchema";

export function createAggregationFields<T>(
  columns: string[] | null | undefined,
  aggregationFn: (column: SQL<any>) => SQL<T>,
  drizzleSchema: any,
  computedColumns?: ComputedColumn[]
): [string, SQL<T>][] | undefined {
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
          drizzleSchema,
          computedColumns,
        })
      ),
    ];
  });
}
