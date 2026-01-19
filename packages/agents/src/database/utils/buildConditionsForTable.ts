import assert from "assert";
import type { Schema } from "@repo/types";
import { tableConditionsSchema } from "@repo/types";

type TableSchema = Schema[number];

/**
 * Build conditions for a single table based on its configured condition and user context.
 * Returns an array of conditions for use in whereAndArray.
 */
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

/**
 * Build a map of table conditions for all relevant tables.
 * This map is passed to the connect package to apply conditions in relation subqueries.
 *
 * @param schema - The full schema containing all tables
 * @param relevantTableNames - Array of table names that may be involved in the query (from join graph)
 * @param userContext - User context containing values for condition resolution
 * @returns A map of tableName -> { column, value } for tables with conditions
 */
export function buildTableConditionsMap(
  schema: Schema,
  relevantTableNames: string[],
  userContext: Record<string, string | number>,
): Record<string, { column: string; value: string | number }> | null {
  const tableConditionsMap: Record<
    string,
    { column: string; value: string | number }
  > = {};

  for (const tableName of relevantTableNames) {
    const tableSchema = schema.find((t) => t.name === tableName);
    if (!tableSchema?.condition) continue;

    const { column: conditionColumn, userContextField } = tableSchema.condition;
    const value = userContext[userContextField.key];

    // Only include if we have a value for this condition
    if (value !== undefined && value !== null) {
      // Look up the actual column to get the dbName
      const schemaColumn = tableSchema.columns.find(
        (c) => c.name === conditionColumn.name,
      );
      const columnDbName = schemaColumn?.dbName ?? conditionColumn.name;

      tableConditionsMap[tableName] = {
        column: columnDbName,
        value: value,
      };
    }
  }

  return Object.keys(tableConditionsMap).length > 0 ? tableConditionsMap : null;
}
