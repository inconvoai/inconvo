import type { SQLCastExpressionAst, SQLComputedColumnAst } from "./querySchema";

export interface SchemaColumn {
  name: string;
  type: string;
  // Optional semantic-model metadata applied from augmentations
  dbName?: string;
  semanticName?: string;
  nullable?: boolean;
  isUnique?: boolean;
  isPrimaryKey?: boolean;
  // STRUCT field support (BigQuery)
  isStructField?: boolean;
  structParent?: string;
  structFieldPath?: string;
}

export interface SchemaComputedColumn {
  name: string;
  ast: SQLComputedColumnAst;
  type?: string | null;
  unit?: string | null;
  notes?: string | null;
}

export interface SchemaColumnConversion {
  column: string;
  ast: SQLCastExpressionAst;
  type?: string | null;
}

export interface SchemaRelation {
  name: string;
  isList: boolean;
  targetTable: string;
  targetSchema?: string;  // For cross-schema relations
  sourceColumns?: string[];
  targetColumns?: string[];
  source?: "FK" | "MANUAL";
  status?: "VALID" | "BROKEN";
  errorTag?: string | null;
}

export interface SchemaTable {
  name: string;
  schema?: string;
  columns: SchemaColumn[];
  // Internal rename maps used for semantic-to-db resolution at query time
  columnRenameMap?: {
    semanticToDb: Record<string, string>;
    dbToSemantic: Record<string, string>;
  };
  relations?: SchemaRelation[];
  computedColumns?: SchemaComputedColumn[];
  columnConversions?: SchemaColumnConversion[];
  virtualTable?: {
    sql: string;
    dialect: DatabaseDialect;
    sourceColumns: Array<{
      sourceName: string;
      name: string;
    }>;
  };
}

export interface SchemaResponse {
  tables: SchemaTable[];
  databaseSchemas?: string[] | null;
}

export type DatabaseDialect =
  | "postgresql"
  | "redshift"
  | "mysql"
  | "mssql"
  | "bigquery";

export interface ConnectionConfig {
  connectionString?: string;
  projectId?: string;
  dataset?: string;
  location?: string;
  dialect: DatabaseDialect;
}
