import assert from "assert";
import type { Schema } from "@repo/types";
import { tableConditionsSchema } from "@repo/types";

type TableSchema = Schema[number];
export function buildConditionsForTable(
  tableSchema: TableSchema,
  userContext: Record<string, string | number>,
) {
  const { columns = [] } = tableSchema;
  assert(columns.length > 0, "Table has no columns");

  const tableCondition = tableSchema.condition;
  if (tableCondition) {
    const value = userContext[tableCondition.userContextField.key];
    assert(value, "Condition value not provided");
    const conditions = [
      {
        column: tableCondition.column.name,
        operator: "equals",
        value: value,
      },
    ];
    return tableConditionsSchema.parse(conditions);
  }
  return null;
}
