import { getColumnFromTable } from "./computedColumns";
import assert from "assert";
import type { SchemaResponse } from "../../types/types";
import type { DatabaseDialect } from "../types";

export function createAggregationFields(
  columns: string[] | null | undefined,
  aggregationFn: (column: any) => any,
  schema: SchemaResponse,
  dialect: DatabaseDialect,
  aliasToTable?: Map<string, string>,
): [string, any][] | undefined {
  if (!columns || columns.length === 0) return undefined;
  return columns.map((columnIdentifier) => {
    const lastDot = columnIdentifier.lastIndexOf(".");
    assert(lastDot !== -1, "Invalid column format for aggregation (not table.column)");
    const tableOrAlias = columnIdentifier.slice(0, lastDot);
    const columnName = columnIdentifier.slice(lastDot + 1);
    const tableName = aliasToTable?.get(tableOrAlias) ?? tableOrAlias;
    return [
      `${tableOrAlias}.${columnName}`,
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
