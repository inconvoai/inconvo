import {
  createTableRelationsHelpers,
  getTableName,
  is,
  Relations,
  Table,
} from "drizzle-orm";
import { getTableConfig } from "drizzle-orm/gel-core";

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

export function getTablePrimaryKey(table: Table) {
  const { columns } = getTableConfig(table);
  for (const column of columns) {
    if (column.primary) {
      return column.name;
    }
  }
  throw new Error("Table does not have a primary key");
}
