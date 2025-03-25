import assert from "assert";
import { loadDrizzleSchema } from "~/util/loadDrizzleSchema";

const tables = await loadDrizzleSchema();

export function buildDrizzleRelationalSelect(
  table: string,
  columns: Record<string, string[] | null>
) {
  const selectObject: Record<string, any> = {};

  Object.keys(columns).forEach((tableName) => {
    const columnNames = columns[tableName];
    if (!columnNames) return;

    const tableParts = tableName.split(".");
    const current = selectObject;

    // Traverse to the correct level for each part, skipping the first table name
    tableParts.slice(1).forEach((part, index) => {
      // If it's the last part, add the columns under "select"
      if (index === tableParts.length - 2) {
        current[part] = columnNames.reduce<Record<string, any>>(
          (acc, columnName) => {
            acc[columnName] = tables[table][columnName];
            return acc;
          },
          {}
        );
      }
    });

    // Directly add columns under "selectObject" if there's only one table part
    if (tableParts.length === 1) {
      assert(
        table === tableParts[0],
        "Column base table must match the starting table"
      );
      columnNames.forEach((columnName) => {
        selectObject[columnName] = tables[table][columnName];
      });
    }
  });

  return selectObject;
}
