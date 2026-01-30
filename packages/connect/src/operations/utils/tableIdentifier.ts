import type { DatabaseDialect } from "../../types/types";

/**
 * Get a schema-qualified table identifier for SQL queries.
 * When tableSchema is provided, returns "schema.table" format.
 * When tableSchema is null/undefined, returns just the table name.
 *
 * All supported dialects (PostgreSQL, Redshift, MySQL, MS SQL, BigQuery)
 * use the same schema.table format.
 *
 * @param tableName - The table name
 * @param tableSchema - The schema name (optional)
 * @param _dialect - The database dialect (currently unused but available for future dialect-specific handling)
 * @returns The schema-qualified table identifier
 */
export function getTableIdentifier(
  tableName: string,
  tableSchema: string | null | undefined,
  _dialect: DatabaseDialect,
): string {
  if (!tableSchema) {
    return tableName;
  }
  // All supported dialects use schema.table format
  return `${tableSchema}.${tableName}`;
}
