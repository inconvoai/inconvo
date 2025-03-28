import {
  createTableRelationsHelpers,
  getTableName,
  is,
  Relations,
  Table,
} from "drizzle-orm";

export function getRelationsForTable(
  tableName: string,
  drizzleTables: Record<string, any>
) {
  for (const [key, value] of Object.entries(drizzleTables)) {
    if (is(value, Relations)) {
      const schemaTableName = getTableName(value.table);
      if (schemaTableName === tableName) {
        return value.config(
          createTableRelationsHelpers(value.table)
        ) as unknown as Relations;
      }
    }
  }
  throw new Error(`Relations for ${tableName} not found in schema`);
}

export function getColumnsForTables(table: Table) {
  // @ts-expect-error - Drizzle doesnt like this
  return table[Table.Symbol.Columns] as typeof table._.columns;
}

export function getAUniqueKeyInTable(table: Table) {
  const columns = getColumnsForTables(table);
  for (const [key, value] of Object.entries(columns)) {
    if (value.isUnique || value.primary) {
      return value.name;
    }
  }

  throw new Error("Table does not have a unique key");
}

export function getRelatedTableNameFromPath(
  path: string[],
  tables: Record<string, any>
) {
  if (path.length < 2) {
    if (!path[0]) {
      throw new Error("Path is empty");
    }
    return path[0];
  }
  let currentTable = path[0];

  for (let i = 0; i < path.length - 1; i++) {
    const relations = getRelationsForTable(currentTable, tables);
    let found = false;
    for (const [key, relation] of Object.entries(relations)) {
      if (relation.fieldName === path[i + 1]) {
        currentTable = relation.referencedTableName;
        found = true;
        break;
      }
    }
    if (!found) {
      throw new Error(`Relationship ${path[i]} -> ${path[i + 1]} not found`);
    }
  }

  return currentTable;
}
