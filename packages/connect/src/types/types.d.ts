import type {
  SQLCastExpressionAst,
  SQLComputedColumnAst,
} from "./querySchema";

export interface SchemaColumn {
  name: string;
  type: string;
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
  relations?: SchemaRelation[];
  computedColumns?: SchemaComputedColumn[];
  columnConversions?: SchemaColumnConversion[];
}

export interface SchemaResponse {
  tables: SchemaTable[];
  databaseSchema?: string | null;
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
