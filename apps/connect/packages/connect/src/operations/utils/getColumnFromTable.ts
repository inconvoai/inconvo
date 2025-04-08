import { getTableName } from "drizzle-orm";
import { ComputedColumn } from "~/types/querySchema";
import { generateComputedColumnAsSQL } from "./generateCopmutedColumnSql";
import { tryCatchSync } from "~/util/tryCatch";

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

  if (tableSchema[columnName]) {
    return tableSchema[columnName];
  }

  if (computedColumns) {
    for (const computedColumn of computedColumns) {
      if (computedColumn.name === columnName) {
        return generateComputedColumnAsSQL(
          computedColumn.ast,
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

export function getColumnFromCTE({
  columnName,
  cte,
}: {
  columnName: string;
  cte: any;
}) {
  const { data: column } = tryCatchSync(() => cte[columnName]);
  if (column) {
    return column;
  }
}
