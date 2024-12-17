import assert from "assert";

export function buildRelationalSelect(
  table: string,
  columns: Record<string, string[] | null>
) {
  const selectObject: Record<string, any> = {};

  Object.keys(columns).forEach((tableName) => {
    const columnNames = columns[tableName];
    if (!columnNames) return;

    const tableParts = tableName.split(".");
    let current = selectObject;

    // Traverse to the correct level for each part, skipping the first table name
    tableParts.slice(1).forEach((part, index) => {
      // If it's the last part, add the columns under "select"
      if (index === tableParts.length - 2) {
        current[part] = {
          select: columnNames.reduce<Record<string, boolean>>(
            (acc, columnName) => {
              acc[columnName] = true;
              return acc;
            },
            {}
          ),
        };
      } else {
        // Otherwise, continue nesting "select" objects as needed
        if (!current[part]) current[part] = { select: {} };
        current = current[part].select;
      }
    });

    // Directly add columns under "selectObject" if there's only one table part
    if (tableParts.length === 1) {
      assert(
        table === tableParts[0],
        "Column base table must match the starting table"
      );
      columnNames.forEach((columnName) => {
        selectObject[columnName] = true;
      });
    }
  });

  return selectObject;
}
