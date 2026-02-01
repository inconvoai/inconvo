import { Kysely } from "kysely";
import type { SchemaResponse } from "../../types/types";
import type { DatabaseDialect } from "../types";

export function getRelatedTableNameFromPath(
  path: string[],
  schema: SchemaResponse,
): string {
  if (path.length < 2) {
    if (!path[0]) {
      throw new Error("Path is empty");
    }
    return path[0];
  }

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

export function getAUniqueKeyInTable(
  tableName: string,
  schema: SchemaResponse,
): string {
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
  dialect: DatabaseDialect,
): Kysely<DB> {
  const schemas = schema.databaseSchemas;
  // Only use withSchema when exactly one schema is configured
  // For multiple schemas, rely on fully-qualified table names (tableSchema on queries)
  if (!schemas?.length || schemas.length > 1 || dialect === "bigquery") {
    return db;
  }
  return db.withSchema(schemas[0]!);
}
