import type { SchemaResponse } from "../types/types.d";

/**
 * Supported database dialects.
 * This is defined separately from env.ts to avoid circular dependencies
 * and to allow Lambda to pass dialect without environment variables.
 */
export type DatabaseDialect =
  | "postgresql"
  | "redshift"
  | "mysql"
  | "mssql"
  | "bigquery";

/**
 * Context passed to all operations.
 * Contains schema and dialect information needed for query building.
 *
 * For AppRunner: dialect comes from env.DATABASE_DIALECT
 * For Lambda: dialect comes from Secrets Manager via loadSchemaAndAugmentations
 */
export interface OperationContext {
  /** The database schema with tables, columns, relations, and augmentations */
  schema: SchemaResponse;
  /** The database dialect for SQL generation */
  dialect: DatabaseDialect;
}
