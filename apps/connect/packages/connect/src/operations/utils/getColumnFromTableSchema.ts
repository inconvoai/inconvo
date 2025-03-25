import { Table } from "drizzle-orm";

export function getColumnFromTableSchema(
  tableSchema: Table,
  columnName: string
) {
  // @ts-expect-error
  const columns = tableSchema[
    // @ts-expect-error
    Table.Symbol.Columns
  ] as typeof tableSchema._.columns;
  for (const [key, value] of Object.entries(columns)) {
    if (value.name === columnName) {
      return value;
    }
  }
  throw new Error(
    `Column ${columnName} not found in table ${tableSchema._.name}`
  );
}
