import { Table } from "drizzle-orm";

export function getColumnFromTableSchema(
  tableSchema: Table,
  columnName: string
) {
  //@ts-expect-error
  const columns = tableSchema[Table.Symbol.Columns];
  for (const [key, value] of Object.entries(columns)) {
    //@ts-expect-error
    if (value["name"] === columnName) {
      return value;
    }
  }
  throw new Error(`Column ${columnName} not found in table schema`);
}
