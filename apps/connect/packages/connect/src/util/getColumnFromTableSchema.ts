import { Table } from "drizzle-orm";

export function getColumnFromTableSchema(
  tableSchema: Table,
  columnName: string
) {
  const columns = tableSchema[Table.Symbol.Columns];
  for (const [key, value] of Object.entries(columns)) {
    if (value["name"] === columnName) {
      return value;
    }
  }
  throw new Error(`Column ${columnName} not found in table schema`);
}
