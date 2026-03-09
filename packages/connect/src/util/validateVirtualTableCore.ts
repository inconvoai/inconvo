import { sql, type Kysely } from "kysely";
import { Client, type FieldDef } from "pg";
import type { DatabaseDialect } from "../operations/types";
import type { BigQueryQuerySchemaField } from "../dialects/bigquery";
import type {
  ValidateVirtualTableResponse,
  ValidateVirtualTableColumn,
} from "@repo/types";
import { QueryExecutionError } from "./queryErrors";
import {
  buildVirtualTableProbeSql,
  executeVirtualTableProbeQuery,
  findInvalidVirtualTableColumnIdentifier,
  normalizeVirtualTableSql,
  validateVirtualTableSqlStatic,
} from "./virtualTableSqlValidation";

export interface ValidateVirtualTableCoreInput {
  sql: string;
  dialect: DatabaseDialect;
  requestDialect?: string;
  previewLimit: number;
  db: Kysely<unknown>;
  pgConnectionString?: string;
}

export type VirtualTableProbeQueryResult = {
  rows?: unknown[];
  schemaFields?: BigQueryQuerySchemaField[];
};

export type VirtualTableInferenceError = {
  kind: "ambiguous_column_reference";
  columnName: string | null;
};

export type InferColumnsFromDbResult =
  | {
      kind: "columns";
      columns: ValidateVirtualTableColumn[] | null;
    }
  | {
      kind: "error";
      error: VirtualTableInferenceError;
    };

const COMMON_PG_TYPE_NAMES_BY_OID = new Map<number, string>([
  [16, "bool"],
  [17, "bytea"],
  [20, "int8"],
  [21, "int2"],
  [23, "int4"],
  [25, "text"],
  [114, "json"],
  [700, "float4"],
  [701, "float8"],
  [1000, "bool[]"],
  [1005, "int2[]"],
  [1007, "int4[]"],
  [1009, "text[]"],
  [1015, "varchar[]"],
  [1016, "int8[]"],
  [1021, "float4[]"],
  [1022, "float8[]"],
  [1042, "bpchar"],
  [1043, "varchar"],
  [1082, "date"],
  [1083, "time"],
  [1114, "timestamp"],
  [1115, "timestamp[]"],
  [1182, "date[]"],
  [1183, "time[]"],
  [1184, "timestamptz"],
  [1185, "timestamptz[]"],
  [1231, "numeric[]"],
  [1700, "numeric"],
  [199, "json[]"],
  [2950, "uuid"],
  [2951, "uuid[]"],
  [3802, "jsonb"],
  [3807, "jsonb[]"],
]);

/**
 * Map a raw SQL type name (from information_schema or catalog) to our simplified
 * logical type, matching the same logic used by buildSchemaFromDb for real tables.
 */
function mapSqlTypeToSimpleType(dataType: string): string {
  const type = (dataType || "").toLowerCase();

  if (
    type.includes("int") ||
    type.includes("serial") ||
    type.includes("numeric") ||
    type.includes("decimal") ||
    type.includes("float") ||
    type.includes("double") ||
    type.includes("real") ||
    type.includes("money")
  ) {
    return "number";
  }

  if (type.includes("bool")) return "boolean";
  if (type.includes("date") || type.includes("time")) return "DateTime";
  if (type.includes("json")) return "json";
  if (type.includes("uuid")) return "string";
  if (
    type.includes("char") ||
    type.includes("text") ||
    type.includes("varchar")
  ) {
    return "string";
  }

  return "string";
}

function mapBigQuerySchemaTypeToSimpleType(dataType: string): string {
  const type = (dataType || "").toLowerCase();

  if (
    type === "record" ||
    type.startsWith("struct") ||
    type.startsWith("array")
  ) {
    return "json";
  }

  return mapSqlTypeToSimpleType(dataType);
}

function inferColumnsViaBigQuerySchemaFields(
  schemaFields: BigQueryQuerySchemaField[] | undefined,
): ValidateVirtualTableColumn[] | null {
  if (!schemaFields || schemaFields.length === 0) return null;

  const columns: ValidateVirtualTableColumn[] = [];
  for (const field of schemaFields) {
    if (!field || typeof field.name !== "string") {
      return null;
    }

    const mode =
      typeof field.mode === "string" ? field.mode.toUpperCase() : undefined;
    const nullable =
      mode === "NULLABLE"
        ? true
        : mode === "REQUIRED" || mode === "REPEATED"
          ? false
          : null;

    columns.push({
      sourceName: field.name,
      type: mapBigQuerySchemaTypeToSimpleType(
        typeof field.type === "string" ? field.type : "",
      ),
      nullable,
    });
  }

  return columns;
}

function dbTypeInferenceFailureMessage(dialect: DatabaseDialect): string {
  if (dialect === "postgresql" || dialect === "redshift") {
    return `Could not infer virtual-table column types from ${dialect} metadata.`;
  }

  if (dialect === "mssql") {
    return "Could not infer virtual-table column types from MSSQL metadata (sys.dm_exec_describe_first_result_set). Check query compatibility/permissions and try again.";
  }

  if (dialect === "bigquery") {
    return "Could not infer virtual-table column types from BigQuery query schema metadata. Try again; if it persists, this query may be returning metadata the connector cannot parse yet.";
  }

  if (dialect === "mysql") {
    return "Virtual tables are not yet supported on MySQL.";
  }

  return `Could not infer virtual-table column types from ${dialect} metadata.`;
}

function duplicateColumnNameMessage(columnName: string): string {
  return `Duplicate column name "${columnName}". This usually happens when joining tables that share a column name. Use explicit column aliases to resolve this, e.g. SELECT orders.${columnName} AS orders_${columnName}, products.${columnName} AS products_${columnName}, ...`;
}

function ambiguousColumnReferenceMessage(columnName: string | null): string {
  const quotedColumn = columnName ? `"${columnName}"` : "a joined column";
  return `Ambiguous column reference detected for ${quotedColumn}. This usually happens when joining tables that share a column name. Use explicit column aliases to resolve this, e.g. SELECT orders.id AS orders_id, products.id AS products_id, ...`;
}

function getErrorMessage(error: unknown): string {
  if (error instanceof Error) {
    return error.message;
  }

  if (
    typeof error === "object" &&
    error !== null &&
    "message" in error &&
    typeof (error as { message?: unknown }).message === "string"
  ) {
    return (error as { message: string }).message;
  }

  return "";
}

function getErrorCode(error: unknown): string | null {
  if (
    typeof error === "object" &&
    error !== null &&
    "code" in error &&
    typeof (error as { code?: unknown }).code === "string"
  ) {
    return (error as { code: string }).code;
  }

  return null;
}

function extractDbErrorDetails(error: unknown): {
  code?: string;
  detail?: string;
  hint?: string;
} {
  if (!error || typeof error !== "object") {
    return {};
  }

  const record = error as Record<string, unknown>;
  return {
    code: typeof record.code === "string" ? record.code : undefined,
    detail: typeof record.detail === "string" ? record.detail : undefined,
    hint: typeof record.hint === "string" ? record.hint : undefined,
  };
}

function extractAmbiguousColumnName(message: string): string | null {
  const quotedMatch = message.match(/column reference "([^"]+)" is ambiguous/iu);
  if (quotedMatch?.[1]) return quotedMatch[1];

  const unquotedMatch = message.match(/column reference ([^\s]+) is ambiguous/iu);
  if (unquotedMatch?.[1]) return unquotedMatch[1];

  return null;
}

export function classifyVirtualTableInferenceError(
  error: unknown,
): VirtualTableInferenceError | null {
  const code = getErrorCode(error);
  const message = getErrorMessage(error);
  const normalizedMessage = message.toLowerCase();
  const isAmbiguousByMessage =
    normalizedMessage.includes("column reference") &&
    normalizedMessage.includes("ambiguous");

  if (code === "42702" || isAmbiguousByMessage) {
    return {
      kind: "ambiguous_column_reference",
      columnName: extractAmbiguousColumnName(message),
    };
  }

  return null;
}

function buildVirtualTableMetadataSql(userSql: string): string {
  const normalized = normalizeVirtualTableSql(userSql);
  return `SELECT * FROM (${normalized}) AS __inconvo_vt_metadata_probe LIMIT 0`;
}

function getPgFieldTypeName(
  field: FieldDef,
  typeNameByOid: Map<number, string>,
): string {
  if (
    typeof field.dataTypeID === "number" &&
    Number.isFinite(field.dataTypeID)
  ) {
    return typeNameByOid.get(field.dataTypeID) ?? "";
  }

  return "";
}

async function loadPgTypeNamesForFields(
  client: Client,
  fields: FieldDef[],
): Promise<Map<number, string>> {
  const typeNameByOid = new Map(COMMON_PG_TYPE_NAMES_BY_OID);
  const unresolvedOids = Array.from(
    new Set(
      fields
        .map((field) =>
          typeof field.dataTypeID === "number" && Number.isFinite(field.dataTypeID)
            ? field.dataTypeID
            : null,
        )
        .filter((oid): oid is number => oid !== null && !typeNameByOid.has(oid)),
    ),
  );

  if (unresolvedOids.length === 0) {
    return typeNameByOid;
  }

  try {
    const typeRows = await client.query<{ oid: number; typname: string }>(
      `SELECT oid::int AS oid, typname
       FROM pg_catalog.pg_type
       WHERE oid = ANY($1::oid[])`,
      [unresolvedOids],
    );

    for (const row of typeRows.rows) {
      typeNameByOid.set(Number(row.oid), row.typname);
    }
  } catch {
    // Fall back to the built-in OID map when catalog access is restricted or incomplete.
  }

  return typeNameByOid;
}

async function inferColumnsViaPgResultMetadata(
  userSql: string,
  connectionString: string,
): Promise<InferColumnsFromDbResult> {
  const metadataSql = buildVirtualTableMetadataSql(userSql);
  const client = new Client({ connectionString });

  try {
    await client.connect();
    const result = await client.query({
      text: metadataSql,
      rowMode: "array",
    });
    const fields = result.fields ?? [];
    if (fields.length === 0) {
      return { kind: "columns", columns: null };
    }

    const typeNameByOid = await loadPgTypeNamesForFields(client, fields);

    return {
      kind: "columns",
      columns: fields.map((field) => ({
        sourceName: field.name,
        type: mapSqlTypeToSimpleType(getPgFieldTypeName(field, typeNameByOid)),
        nullable: null,
      })),
    };
  } catch (error) {
    const inferredError = classifyVirtualTableInferenceError(error);
    if (inferredError) {
      return {
        kind: "error",
        error: inferredError,
      };
    }

    throw new QueryExecutionError(
      {
        type: "query_execution",
        message:
          getErrorMessage(error) ||
          "Query execution failed during virtual table validation.",
        sql: metadataSql,
        params: [],
        operation: "validateVirtualTable",
        ...extractDbErrorDetails(error),
      },
      error,
    );
  } finally {
    await client.end().catch(() => undefined);
  }
}

/**
 * For PostgreSQL / Redshift: infer column types via pg_typeof().
 * SELECT-only approach: no EXPLAIN and no metadata-only commands.
 *
 * `columnNames` comes from Object.keys(firstRow) and preserves user aliases.
 */
async function inferColumnsViaPgTypeof(
  db: Kysely<unknown>,
  userSql: string,
  columnNames: string[],
): Promise<ValidateVirtualTableColumn[] | null> {
  if (columnNames.length === 0) return null;

  const normalized = normalizeVirtualTableSql(userSql);

  const selectCols = columnNames
    .map((name) => {
      const escaped = name.replace(/"/gu, '""');
      return `pg_typeof("${escaped}")::text AS "${escaped}"`;
    })
    .join(", ");

  const typeQuery = `SELECT ${selectCols} FROM (${normalized}) AS __inconvo_vt_type_probe LIMIT 1`;
  const result = await sql.raw(typeQuery).execute(db);
  const rows = (result.rows ?? []) as Record<string, string>[];

  if (rows.length === 0) return null;
  const typeRow = rows[0];
  if (!typeRow) return null;

  return columnNames.map((name) => ({
    sourceName: name,
    type: mapSqlTypeToSimpleType(typeRow[name] ?? ""),
    nullable: null,
  }));
}

/**
 * For MSSQL: infer column types via sys.dm_exec_describe_first_result_set().
 * This DMV describes the output columns of a SQL statement without executing it.
 * Completely read-only — no DDL, no temp tables.
 */
async function inferColumnsViaMssqlDescribe(
  db: Kysely<unknown>,
  userSql: string,
): Promise<ValidateVirtualTableColumn[] | null> {
  const normalized = normalizeVirtualTableSql(userSql);
  // Escape single quotes in the SQL string for use inside the DMV call
  const escapedSql = normalized.replace(/'/gu, "''");

  const typeQuery = `SELECT name, system_type_name FROM sys.dm_exec_describe_first_result_set(N'${escapedSql}', NULL, 0) ORDER BY column_ordinal`;

  const result = await sql.raw(typeQuery).execute(db);
  const rows = (result.rows ?? []) as Array<{
    name: string;
    system_type_name: string;
  }>;

  if (rows.length === 0) return null;

  return rows.map((row) => ({
    sourceName: row.name,
    type: mapSqlTypeToSimpleType(row.system_type_name),
    nullable: null,
  }));
}

/**
 * Infer column types by asking the database for real SQL types.
 * Each dialect uses the most appropriate introspection mechanism:
 *
 *  - PostgreSQL / Redshift: pg_typeof() via SELECT-only query
 *  - MSSQL: sys.dm_exec_describe_first_result_set() — no execution needed
 *  - BigQuery: query result schema metadata (schema.fields from getQueryResults)
 *  - MySQL: not supported (returns null)
 */
async function inferColumnsFromDb(
  db: Kysely<unknown>,
  dialect: DatabaseDialect,
  userSql: string,
  columnNames: string[],
  probeResult?: VirtualTableProbeQueryResult,
): Promise<InferColumnsFromDbResult> {
  try {
    switch (dialect) {
      case "postgresql":
      case "redshift":
        return {
          kind: "columns",
          columns: await inferColumnsViaPgTypeof(db, userSql, columnNames),
        };
      case "mssql":
        return {
          kind: "columns",
          columns: await inferColumnsViaMssqlDescribe(db, userSql),
        };
      case "mysql":
        return { kind: "columns", columns: null };
      case "bigquery":
        return {
          kind: "columns",
          columns: inferColumnsViaBigQuerySchemaFields(probeResult?.schemaFields),
        };
      default:
        return { kind: "columns", columns: null };
    }
  } catch (error) {
    const inferredError = classifyVirtualTableInferenceError(error);
    if (inferredError) {
      return {
        kind: "error",
        error: inferredError,
      };
    }
    // Any non-classified failure → let the caller show a generic inference error.
    return { kind: "columns", columns: null };
  }
}

export interface ValidateVirtualTableCoreDeps {
  inferColumnsFromValidationMetadata: (params: {
    dialect: DatabaseDialect;
    userSql: string;
    pgConnectionString?: string;
  }) => Promise<InferColumnsFromDbResult | null>;
  executeProbeQuery: (
    db: Kysely<unknown>,
    dialect: DatabaseDialect,
    probeSql: string,
  ) => Promise<VirtualTableProbeQueryResult>;
  inferColumnsFromDb: (
    db: Kysely<unknown>,
    dialect: DatabaseDialect,
    userSql: string,
    columnNames: string[],
    probeResult?: VirtualTableProbeQueryResult,
  ) => Promise<InferColumnsFromDbResult>;
}

const defaultValidateVirtualTableCoreDeps: ValidateVirtualTableCoreDeps = {
  inferColumnsFromValidationMetadata: async ({
    dialect,
    userSql,
    pgConnectionString,
  }) => {
    if (
      (dialect === "postgresql" || dialect === "redshift") &&
      pgConnectionString
    ) {
      return await inferColumnsViaPgResultMetadata(userSql, pgConnectionString);
    }

    return null;
  },
  executeProbeQuery: async (db, dialect, probeSql) => {
    return (await executeVirtualTableProbeQuery(
      db,
      dialect,
      probeSql,
    )) as VirtualTableProbeQueryResult;
  },
  inferColumnsFromDb,
};

/**
 * Core virtual table validation logic, shared between Express and Hono handlers.
 * Returns a fully-typed ValidateVirtualTableResponse.
 * Throws QueryExecutionError if the probe query fails (callers handle this).
 */
export async function validateVirtualTableCore(
  input: ValidateVirtualTableCoreInput,
  deps: ValidateVirtualTableCoreDeps = defaultValidateVirtualTableCoreDeps,
): Promise<ValidateVirtualTableResponse> {
  const {
    sql: userSql,
    dialect,
    requestDialect,
    previewLimit,
    db,
    pgConnectionString,
  } = input;

  const staticError = validateVirtualTableSqlStatic(userSql, dialect);
  if (staticError) {
    return { ok: false, error: { message: staticError } };
  }

  if (requestDialect && requestDialect !== dialect) {
    return {
      ok: false,
      error: {
        message: `Dialect mismatch: SQL is marked ${requestDialect} but connector dialect is ${dialect}`,
      },
    };
  }

  const metadataInferResult = await deps.inferColumnsFromValidationMetadata({
    dialect,
    userSql,
    pgConnectionString,
  });
  if (metadataInferResult) {
    if (metadataInferResult.kind === "error") {
      return {
        ok: false,
        error: {
          message: ambiguousColumnReferenceMessage(
            metadataInferResult.error.columnName,
          ),
        },
      };
    }

    const metadataColumns = metadataInferResult.columns;
    if (!metadataColumns) {
      return {
        ok: false,
        error: {
          message: dbTypeInferenceFailureMessage(dialect),
        },
      };
    }

    return validateInferredColumns(metadataColumns);
  }

  const probeSql = buildVirtualTableProbeSql(userSql, dialect, previewLimit);
  const result = await deps.executeProbeQuery(
    db,
    dialect,
    probeSql,
  );
  const probeRows = (result.rows ?? []) as Record<string, unknown>[];

  // Primary: ask the database for real column types.
  // Each dialect uses its best introspection mechanism.
  const firstRow = probeRows[0];
  const columnNames = firstRow ? Object.keys(firstRow) : [];
  const inferResult = await deps.inferColumnsFromDb(
    db,
    dialect,
    userSql,
    columnNames,
    result,
  );
  if (inferResult.kind === "error") {
    return {
      ok: false,
      error: {
        message: ambiguousColumnReferenceMessage(inferResult.error.columnName),
      },
    };
  }
  const columns = inferResult.columns;

  if (!columns) {
    return {
      ok: false,
      error: {
        message: dbTypeInferenceFailureMessage(dialect),
      },
    };
  }

  return validateInferredColumns(columns);
}

function validateInferredColumns(
  columns: ValidateVirtualTableColumn[],
): ValidateVirtualTableResponse {
  // Check for duplicate column names (e.g. from SELECT * with JOINs)
  const seenNames = new Set<string>();
  for (const col of columns) {
    if (seenNames.has(col.sourceName)) {
      return {
        ok: false,
        error: {
          message: duplicateColumnNameMessage(col.sourceName),
        },
      };
    }
    seenNames.add(col.sourceName);
  }

  const invalidColumnName = findInvalidVirtualTableColumnIdentifier(columns);
  if (invalidColumnName) {
    return {
      ok: false,
      error: {
        message: `Unsupported virtual-table column name "${invalidColumnName}". Alias columns to simple names using only letters, numbers, and underscores (starting with a letter or underscore).`,
      },
    };
  }

  return { ok: true, columns };
}
