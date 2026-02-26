import * as nodeSqlParserNs from "node-sql-parser";
import { sql, type Kysely } from "kysely";
import type { ValidateVirtualTableColumn } from "@repo/types";
import type { DatabaseDialect } from "../operations/types";

type ParserLike = {
  astify: (sql: string, options?: unknown) => unknown;
};

const ParserCtor = (
  (nodeSqlParserNs as { Parser?: new () => ParserLike }).Parser ??
  (nodeSqlParserNs as { default?: { Parser?: new () => ParserLike } }).default
    ?.Parser
);

if (!ParserCtor) {
  throw new Error("node-sql-parser Parser export not found");
}

const parser = new ParserCtor();
const SIMPLE_SQL_IDENTIFIER_REGEX = /^[A-Za-z_][A-Za-z0-9_]*$/u;

function toParserDatabase(dialect: DatabaseDialect): string {
  switch (dialect) {
    case "postgresql":
      return "PostgreSQL";
    case "redshift":
      return "Redshift";
    case "mysql":
      return "MySQL";
    case "mssql":
      return "TransactSQL";
    case "bigquery":
      return "BigQuery";
  }
}

export function normalizeVirtualTableSql(input: string): string {
  const trimmed = input.trim();
  return trimmed.replace(/;+\s*$/u, "");
}

export function validateVirtualTableSqlStatic(
  sqlText: string,
  dialect: DatabaseDialect,
): string | null {
  const normalized = normalizeVirtualTableSql(sqlText);
  if (!normalized) return "SQL is required";

  let ast: unknown;
  try {
    ast = parser.astify(normalized, {
      database: toParserDatabase(dialect),
    } as any);
  } catch (error) {
    const message = error instanceof Error ? error.message : "Parse failed";
    return `Invalid SQL syntax: ${message}`;
  }

  const statements = Array.isArray(ast) ? ast : [ast];
  if (statements.length !== 1) {
    return "Only a single SQL statement is allowed";
  }

  const statement = statements[0] as { type?: unknown; into?: unknown } | undefined;
  const type = typeof statement?.type === "string" ? statement.type.toLowerCase() : "";
  if (type !== "select") {
    return "Only read-only SELECT statements (including WITH ... SELECT) are supported";
  }
  const into = statement?.into as { expr?: unknown; position?: unknown } | undefined;
  if (into && (into.expr != null || into.position != null)) {
    return "SELECT INTO is not supported for virtual tables because validation must be read-only";
  }

  return null;
}

export function findInvalidVirtualTableColumnIdentifier(
  columns: ValidateVirtualTableColumn[],
): string | null {
  for (const column of columns) {
    if (!SIMPLE_SQL_IDENTIFIER_REGEX.test(column.sourceName)) {
      return column.sourceName;
    }
  }
  return null;
}

function supportsReadOnlyProbeTransaction(dialect: DatabaseDialect): boolean {
  switch (dialect) {
    case "postgresql":
    case "redshift":
    case "mysql":
      return true;
    case "mssql":
    case "bigquery":
      return false;
  }
}

export async function executeVirtualTableProbeQuery(
  db: Kysely<unknown>,
  dialect: DatabaseDialect,
  probeSql: string,
): Promise<{ rows?: unknown[] }> {
  if (!supportsReadOnlyProbeTransaction(dialect)) {
    return (await sql.raw(probeSql).execute(db)) as { rows?: unknown[] };
  }

  return (await db
    .transaction()
    .setAccessMode("read only")
    .execute(async (trx) => {
      return (await sql.raw(probeSql).execute(trx)) as { rows?: unknown[] };
    })) as { rows?: unknown[] };
}

export function buildVirtualTableProbeSql(
  userSql: string,
  dialect: DatabaseDialect,
  previewLimit: number,
): string {
  const normalized = normalizeVirtualTableSql(userSql);
  if (dialect === "mssql") {
    return `SELECT TOP (${previewLimit}) * FROM (${normalized}) AS __inconvo_vt_probe`;
  }
  return `SELECT * FROM (${normalized}) AS __inconvo_vt_probe LIMIT ${previewLimit}`;
}
