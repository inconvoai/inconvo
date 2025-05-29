import assert from "assert";
import type { Schema } from "~/server/db/schema";

export function generateJoinedTables(
  schema: Schema,
  startingTableName: string,
  maxDepth: number
) {
  const tableSchema = schema.find((table) => table.name === startingTableName);
  assert(tableSchema, `Table ${startingTableName} not found in schema`);

  const paths: [string, string][][] = [];
  for (const relation of tableSchema.outwardRelations ?? []) {
    const relationName = relation.name;
    paths.push([
      [startingTableName, startingTableName],
      [relationName, relation.targetTable.name],
    ]);
  }

  for (let i = 1; i < maxDepth; i++) {
    const pathsToExtend = paths.filter((path) => path.length == i + 1);
    for (const path of pathsToExtend) {
      const endTableSchema = schema.find(
        (table) => table.name === path.at(-1)?.[1]
      );
      const tablesInPath = path.map((pair) => pair[1]);
      assert(endTableSchema, "Table does not exist");
      for (const relation of endTableSchema.outwardRelations ?? []) {
        const relationName = relation.name;
        if (tablesInPath.includes(relation.targetTable.name)) continue;
        if (relation.targetTable.name === startingTableName) continue;
        paths.push([...path, [relationName, relation.targetTable.name]]);
      }
    }
  }

  const iqlPaths: Record<string, string> = {
    [startingTableName]: startingTableName,
  };

  paths.forEach(
    (path) =>
      (iqlPaths[
        path
          .map((rel) => rel[0])
          .flat()
          .join(".")
      ] = path.at(-1)![1])
  );

  const tables = [...paths.map((path) => path.at(-1)![1]), startingTableName];
  const uniqueTableNames = Array.from(new Set(tables));

  return { iqlPaths, uniqueTableNames };
}

export function findRelationsToAColumn(
  schema: Schema,
  columnName: string,
  tableName: string
) {
  const table = schema.find((table) => table.name === tableName);
  assert(table, `Table ${tableName} not found in schema`);
  const columnObject = table.columns.find((col) => col.name === columnName);
  assert(columnObject, `Column ${columnName} not found in table ${tableName}`);

  if (columnObject.relation) {
    const result: Record<string, string[]> = {};

    for (const rel of columnObject.relation) {
      const targetTableName = rel.relation.targetTable.name;
      if (targetTableName) {
        const targetColumns = schema.find(
          (table) => table.name === targetTableName
        )?.columns;

        if (!targetColumns) {
          throw new Error(`Table ${targetTableName} not found in schema`);
        }

        result[targetTableName] = targetColumns.map((col) => col.name);
      }
    }
    return Object.keys(result).length > 0 ? result : null;
  }

  return null;
}
