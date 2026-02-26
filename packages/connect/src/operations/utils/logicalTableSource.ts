import { sql } from "kysely";
import type { SchemaResponse, DatabaseDialect, SchemaTable } from "../../types/types";
import { getTableIdentifier } from "./tableIdentifier";

function findTable(schema: SchemaResponse, tableName: string): SchemaTable | undefined {
  return schema.tables.find((t) => t.name === tableName);
}

export function isVirtualTable(schema: SchemaResponse, tableName: string): boolean {
  return !!findTable(schema, tableName)?.virtualTable;
}

function assertSafeIdentifier(name: string, kind: "table" | "column" | "alias"): void {
  if (!/^[A-Za-z_][A-Za-z0-9_]*$/u.test(name)) {
    throw new Error(
      `Unsupported ${kind} identifier for SQL virtual tables: ${name}. Use simple alphanumeric/underscore names in v1.`,
    );
  }
}

function buildAliasedVirtualSql(
  table: SchemaTable & { virtualTable: NonNullable<SchemaTable["virtualTable"]> },
  dialect: DatabaseDialect,
): string {
  const meta = table.virtualTable;
  if (meta.dialect !== dialect) {
    throw new Error(
      `Virtual table ${table.name} is defined for ${meta.dialect} but connection is ${dialect}`,
    );
  }

  const sourceAlias = "__inconvo_vt_src";
  assertSafeIdentifier(sourceAlias, "alias");
  assertSafeIdentifier(table.name, "table");

  const selected = meta.sourceColumns.filter((column) =>
    table.columns.some((c) => c.name === column.name),
  );

  if (selected.length === 0) {
    throw new Error(`Virtual table ${table.name} has no selected columns`);
  }

  const selectList = selected
    .map(({ sourceName, name }) => {
      assertSafeIdentifier(sourceName, "column");
      assertSafeIdentifier(name, "column");
      return `${sourceAlias}.${sourceName} AS ${name}`;
    })
    .join(", ");

  const baseSql = meta.sql.trim().replace(/;+\s*$/u, "");
  return `SELECT ${selectList} FROM (${baseSql}) AS ${sourceAlias}`;
}

// Kysely's selectFrom() only accepts string literals or AliasedExpression<DB, ...>
// in its type signature. When a virtual table subquery is used, we return a
// sql.raw(...).as(alias) expression whose inner type can't be expressed in the
// generic without access to the DB schema type. The `as any` cast at call sites
// is intentional; identifier safety is enforced by assertSafeIdentifier() above.
function buildVirtualSource(
  table: SchemaTable & { virtualTable: NonNullable<SchemaTable["virtualTable"]> },
  dialect: DatabaseDialect,
  sqlAlias?: string,
) {
  const wrapped = buildAliasedVirtualSql(table, dialect);
  const alias = sqlAlias ?? table.name;
  assertSafeIdentifier(alias, "alias");
  return sql.raw(`(${wrapped})`).as(alias);
}

export function resolveBaseSource(args: {
  tableName: string;
  tableSchema?: string | null;
  schema: SchemaResponse;
  dialect: DatabaseDialect;
}) {
  const table = findTable(args.schema, args.tableName);
  if (table?.virtualTable) {
    return {
      source: buildVirtualSource(
        table as SchemaTable & {
          virtualTable: NonNullable<SchemaTable["virtualTable"]>;
        },
        args.dialect,
        args.tableName,
      ),
      isVirtual: true,
    };
  }

  return {
    source: getTableIdentifier(args.tableName, args.tableSchema ?? null, args.dialect),
    isVirtual: false,
  };
}

export function resolveJoinTargetSource(args: {
  tableName: string;
  sqlAlias: string;
  schema: SchemaResponse;
  dialect: DatabaseDialect;
}) {
  const table = findTable(args.schema, args.tableName);
  if (table?.virtualTable) {
    return {
      source: buildVirtualSource(
        table as SchemaTable & {
          virtualTable: NonNullable<SchemaTable["virtualTable"]>;
        },
        args.dialect,
        args.sqlAlias,
      ),
      isVirtual: true,
    };
  }

  const physical = getTableIdentifier(args.tableName, table?.schema, args.dialect);
  if (args.sqlAlias !== args.tableName) {
    assertSafeIdentifier(args.sqlAlias, "alias");
    return {
      source: sql.raw(`${physical} as ${args.sqlAlias}`),
      isVirtual: false,
    };
  }
  return { source: physical, isVirtual: false };
}

