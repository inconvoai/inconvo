import { Column, getTableName, Table } from "drizzle-orm";
import { ComputedColumn } from "~/types/querySchema";
import { generateComputedColumnAsSQL } from "./generateCopmutedColumnSql";

interface GetColumnFromTableParams {
  columnName: string;
  tableName: string;
  drizzleSchema: Record<string, any>;
  computedColumns?: ComputedColumn[];
}

export function getColumnFromTable({
  columnName,
  tableName,
  drizzleSchema,
  computedColumns,
}: GetColumnFromTableParams) {
  const tableSchema = drizzleSchema[tableName];
  const columns = tableSchema[
    // @ts-expect-error
    Table.Symbol.Columns
  ] as Column[];
  for (const [key, value] of Object.entries(columns)) {
    if (value.name === columnName) {
      return value;
    }
  }

  if (computedColumns) {
    for (const computedColumn of computedColumns) {
      if (computedColumn.name === columnName) {
        return generateComputedColumnAsSQL(
          computedColumn.expression,
          getTableName(tableSchema),
          drizzleSchema
        );
      }
    }
  }

  throw new Error(
    `Column ${columnName} not found in table ${getTableName(tableSchema)}`
  );
}
