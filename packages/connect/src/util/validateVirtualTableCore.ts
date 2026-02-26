import { sql, type Kysely } from "kysely";
import type { DatabaseDialect } from "../operations/types";
import type { BigQueryQuerySchemaField } from "../dialects/bigquery";
import type {
  ValidateVirtualTableResponse,
  ValidateVirtualTableColumn,
} from "@repo/types";
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
}

type VirtualTableProbeQueryResult = {
  rows?: unknown[];
  schemaFields?: BigQueryQuerySchemaField[];
};

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
    return `Could not infer virtual-table column types from ${dialect} metadata. This can happen when the validation preview returns zero rows (no column names are available for pg_typeof). Try validating with a query/filter that returns at least one row.`;
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

/**
 * For PostgreSQL / Redshift: infer column types by asking the database directly
 * via pg_typeof(). This runs a single read-only query — no DDL, no temp views.
 *
 * pg_typeof() returns the declared type of each column even when the value is
 * NULL, so we get real SQL types (integer, numeric, double precision, etc.)
 * instead of guessing from JS typeof on driver-returned values.
 */
async function inferColumnsViaPgTypeof(
  db: Kysely<unknown>,
  userSql: string,
  columnNames: string[],
): Promise<ValidateVirtualTableColumn[] | null> {
  if (columnNames.length === 0) return null;

  const normalized = normalizeVirtualTableSql(userSql);

  // Build: SELECT pg_typeof("col1")::text AS "col1", pg_typeof("col2")::text AS "col2", ...
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
 *  - PostgreSQL / Redshift: pg_typeof() — single read-only query
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
): Promise<ValidateVirtualTableColumn[] | null> {
  try {
    switch (dialect) {
      case "postgresql":
      case "redshift":
        return await inferColumnsViaPgTypeof(db, userSql, columnNames);
      case "mssql":
        return await inferColumnsViaMssqlDescribe(db, userSql);
      case "mysql":
        return null;
      case "bigquery":
        return inferColumnsViaBigQuerySchemaFields(probeResult?.schemaFields);
      default:
        return null;
    }
  } catch {
    // Any failure → let the caller decide whether to use a fallback or error.
    return null;
  }
}

/**
 * Core virtual table validation logic, shared between Express and Hono handlers.
 * Returns a fully-typed ValidateVirtualTableResponse.
 * Throws QueryExecutionError if the probe query fails (callers handle this).
 */
export async function validateVirtualTableCore(
  input: ValidateVirtualTableCoreInput,
): Promise<ValidateVirtualTableResponse> {
  const { sql: userSql, dialect, requestDialect, previewLimit, db } = input;

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

  const probeSql = buildVirtualTableProbeSql(userSql, dialect, previewLimit);
  const result = (await executeVirtualTableProbeQuery(
    db,
    dialect,
    probeSql,
  )) as VirtualTableProbeQueryResult;
  const previewRows = (result.rows ?? []) as Record<string, unknown>[];

  // Primary: ask the database for real column types.
  // Each dialect uses its best introspection mechanism (pg_typeof, DMV, etc.)
  const firstRow = previewRows[0];
  const columnNames = firstRow ? Object.keys(firstRow) : [];
  let columns = await inferColumnsFromDb(
    db,
    dialect,
    userSql,
    columnNames,
    result,
  );

  if (!columns) {
    return {
      ok: false,
      error: {
        message: dbTypeInferenceFailureMessage(dialect),
      },
    };
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

  return { ok: true, columns, previewRows };
}
