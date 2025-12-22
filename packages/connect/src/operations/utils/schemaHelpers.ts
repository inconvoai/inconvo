import { Kysely } from "kysely";
import type { SchemaResponse } from "../../types/types";
import { getAugmentedSchema } from "../../util/augmentedSchemaCache";
import { env } from "../../env";

export async function getRelatedTableNameFromPath(
  path: string[],
): Promise<string> {
  if (path.length < 2) {
    if (!path[0]) {
      throw new Error("Path is empty");
    }
    return path[0];
  }

  const schema = await getAugmentedSchema();
  let currentTable = path[0]!;

  for (let i = 0; i < path.length - 1; i++) {
    const table = schema.tables.find((t: any) => t.name === currentTable);
    if (!table) {
      throw new Error(`Table ${currentTable} not found in schema`);
    }

    const nextRelationName = path[i + 1];

    const relations = table.relations || [];
    const directMatch = relations.find(
      (relation: any) => relation.name === nextRelationName,
    );
    const tableMatch = relations.find(
      (relation: any) => relation.targetTable === nextRelationName,
    );
    const relation = directMatch ?? tableMatch;

    if (!relation) {
      throw new Error(`Relationship ${path[i]} -> ${path[i + 1]} not found`);
    }

    currentTable = relation.targetTable;
  }

  return currentTable;
}

export async function getAUniqueKeyInTable(tableName: string): Promise<string> {
  const schema = await getAugmentedSchema();
  const table = schema.tables.find((t: any) => t.name === tableName);

  if (!table) {
    throw new Error(`Table ${tableName} not found in schema`);
  }

  for (const column of table.columns || []) {
    if (column.isUnique || column.isPrimaryKey || column.name === "id") {
      return column.name;
    }
  }

  throw new Error(`Table ${tableName} does not have a unique key`);
}

export function getSchemaBoundDb<DB>(
  db: Kysely<DB>,
  schema: SchemaResponse,
): Kysely<DB> {
  if (!schema.databaseSchema || env.DATABASE_DIALECT === "bigquery") {
    return db;
  }

  return db.withSchema(schema.databaseSchema);
}
