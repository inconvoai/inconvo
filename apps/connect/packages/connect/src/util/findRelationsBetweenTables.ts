import { getTableConfig, AnyPgTable } from "drizzle-orm/pg-core";

export function findRelationsBetweenTables(
  tableA: AnyPgTable,
  tableB: AnyPgTable
): [string, string, boolean] | null {
  const configA = getTableConfig(tableA);
  const configB = getTableConfig(tableB);

  // 1) Check if tableA references tableB via foreignKeys
  for (const fk of configA.foreignKeys) {
    if (!fk) continue;

    if (fk.reference().foreignTable === tableB) {
      const localCol = fk.reference().columns.map((col) => col.name);
      const foreignCol = fk.reference().foreignColumns.map((col) => col.name);
      return [localCol[0], foreignCol[0], true];
    }
  }

  // 2) Otherwise, check if tableB references tableA
  for (const fk of configB.foreignKeys) {
    if (!fk) continue;

    if (fk.reference().foreignTable === tableA) {
      const localCol = fk.reference().columns.map((col) => col.name);
      const foreignCol = fk.reference().foreignColumns.map((col) => col.name);
      return [foreignCol[0], localCol[0], false];
    }
  }

  // 3) If neither references the other directly, return null
  return null;
}
