import { sql, type RawBuilder } from "kysely";
import type {
  LogicalCastType,
  SQLCastExpressionAst,
  SQLComputedColumnAst,
} from "../../types/querySchema";
import type { SchemaResponse } from "../../types/types";
import type { DatabaseDialect } from "../types";

function resolveColumnNameInTable(
  table: SchemaResponse["tables"][number],
  requestedColumnName: string,
): string {
  // Fast path: direct DB column name exists.
  if (table.columns.some((column) => column.name === requestedColumnName)) {
    return requestedColumnName;
  }

  // Semantic rename map takes precedence for renamed columns.
  const mappedDbName = table.columnRenameMap?.semanticToDb[requestedColumnName];
  if (mappedDbName) {
    return mappedDbName;
  }

  // Keep legacy behavior for unknown columns - downstream validation will fail naturally.
  return requestedColumnName;
}

export function getColumnFromTable({
  columnName,
  tableName,
  schema,
  dialect,
  tableAlias,
}: {
  columnName: string;
  tableName: string;
  schema: SchemaResponse;
  dialect: DatabaseDialect;
  tableAlias?: string; // Optional SQL alias for when same table is joined multiple times
}): RawBuilder<unknown> {
  const table = schema.tables.find((t) => t.name === tableName);
  if (!table) {
    throw new Error(`Table not found in schema: ${tableName}`);
  }

  const resolvedColumnName = resolveColumnNameInTable(table, columnName);

  // Use tableAlias for SQL references if provided, otherwise use tableName
  const sqlTableRef = tableAlias ?? tableName;

  // Check if it's a computed column
  const computedColumn = table.computedColumns?.find(
    (cc) => cc.name === columnName,
  );

  if (computedColumn) {
    return generateComputedColumnAsSQL(
      computedColumn.ast,
      sqlTableRef,
      schema,
      dialect,
      false,
      tableName,
    );
  }

  const columnConversion = table.columnConversions?.find(
    (conversion) => conversion.column === resolvedColumnName,
  );

  if (columnConversion) {
    return generateColumnConversionAsSQL(columnConversion.ast, sqlTableRef, dialect);
  }

  return buildColumnReference(sqlTableRef, resolvedColumnName, dialect);
}

function buildColumnReference(
  tableName: string,
  columnName: string,
  dialect: DatabaseDialect,
): RawBuilder<unknown> {
  // BigQuery STRUCT field handling: columns with # separator represent nested STRUCT fields
  // Column name format: "structColumn#fieldPath" or "structColumn#nested#field"
  // Converts to BigQuery SQL: table.structColumn.fieldPath or table.structColumn.nested.field
  if (dialect === "bigquery" && columnName.includes("#")) {
    // Convert # separators back to dots for BigQuery STRUCT field access
    const structPath = columnName.replace(/#/g, ".");
    // Validate structPath contains only safe identifier characters (defense-in-depth)
    // Each segment must be alphanumeric/underscore, no consecutive dots or empty segments
    if (!/^[a-zA-Z0-9_]+(\.[a-zA-Z0-9_]+)*$/.test(structPath)) {
      throw new Error(`Invalid STRUCT field path: ${structPath}`);
    }
    // Use raw SQL for the struct path since it contains dots that are part of field access
    return sql`${sql.ref(tableName)}.${sql.raw(structPath)}`;
  }

  // Regular column
  return sql`${sql.ref(tableName)}.${sql.ref(columnName)}`;
}

function generateColumnConversionAsSQL(
  ast: SQLCastExpressionAst,
  tableName: string,
  dialect: DatabaseDialect,
): RawBuilder<unknown> {
  switch (ast.type) {
    case "column":
      return buildColumnReference(tableName, ast.name, dialect);
    case "value":
      return sql`${ast.value}`;
    case "cast": {
      const inner = generateColumnConversionAsSQL(ast.expression, tableName, dialect);
      const castKeyword = dialect === "bigquery" ? "SAFE_CAST" : "CAST";
      const target = resolveLogicalType(ast.as, dialect);
      return sql`${sql.raw(castKeyword)}(${inner} AS ${sql.raw(target)})`;
    }
    case "coalesce": {
      const expression = generateColumnConversionAsSQL(
        ast.expression,
        tableName,
        dialect,
      );
      const fallback = generateColumnConversionAsSQL(ast.fallback, tableName, dialect);
      return sql`COALESCE(${expression}, ${fallback})`;
    }
    case "brackets": {
      const inner = generateColumnConversionAsSQL(ast.expression, tableName, dialect);
      return sql`(${inner})`;
    }
    default:
      throw new Error(
        `Unexpected state: unknown column conversion AST node. This should never happen unless there is a bug in the type definitions or runtime data. AST: ${JSON.stringify(ast)}`,
      );
  }
}

function generateComputedColumnAsSQL(
  ast: SQLComputedColumnAst,
  tableSqlRef: string,
  schema: SchemaResponse,
  dialect: DatabaseDialect,
  numericRequired: boolean,
  schemaTableName?: string,
): RawBuilder<unknown> {
  switch (ast.type) {
    case "column": {
      // If a conversion exists, use it; otherwise enforce numeric when required
      const table = schemaTableName
        ? schema.tables.find((t) => t.name === schemaTableName)
        : undefined;
      const resolvedColumnName = table
        ? resolveColumnNameInTable(table, ast.name)
        : ast.name;
      const conversion = table?.columnConversions?.find(
        (c) => c.column === resolvedColumnName,
      );
      if (conversion) {
        return generateColumnConversionAsSQL(
          conversion.ast,
          tableSqlRef,
          dialect,
        );
      }
      if (numericRequired) {
        const columnType = table?.columns.find(
          (col) => col.name === resolvedColumnName,
        )?.type;
        if (columnType && columnType.toLowerCase() !== "number") {
          throw new Error(
            `Computed column requires numeric input but '${resolvedColumnName}' is type '${columnType}' without a conversion`,
          );
        }
      }
      return buildColumnReference(tableSqlRef, resolvedColumnName, dialect);
    }

    case "value":
      return sql`${ast.value}`;

    case "function": {
      const [arg] = ast.arguments;
      const compiledArg = generateComputedColumnAsSQL(
        arg,
        tableSqlRef,
        schema,
        dialect,
        true,
        schemaTableName,
      );
      return sql`${sql.raw(ast.name)}(${compiledArg})`;
    }

    case "operation": {
      const operands = ast.operands.map((operand) =>
        generateComputedColumnAsSQL(
          operand,
          tableSqlRef,
          schema,
          dialect,
          true,
          schemaTableName,
        ),
      );

      switch (ast.operator) {
        case "+":
          return operands.reduce((acc, op) => sql`${acc} + ${op}`);
        case "-":
          return operands.reduce((acc, op) => sql`${acc} - ${op}`);
        case "*":
          return operands.reduce((acc, op) => sql`${acc} * ${op}`);
        case "/":
          return operands.reduce((acc, op) => sql`${acc} / ${op}`);
        case "%":
          return operands.reduce((acc, op) => sql`${acc} % ${op}`);
        default:
          throw new Error(`Unknown operator: ${ast.operator}`);
      }
    }

    case "brackets": {
      const inner = generateComputedColumnAsSQL(
        ast.expression,
        tableSqlRef,
        schema,
        dialect,
        numericRequired,
        schemaTableName,
      );
      return sql`(${inner})`;
    }

    default:
      throw new Error(`Unknown AST node type: ${(ast as any).type}`);
  }
}

function resolveLogicalType(logical: LogicalCastType, dialect: DatabaseDialect): string {
  const DIALECT_TYPE_MAP: Record<
    DatabaseDialect,
    Partial<Record<LogicalCastType, string>>
  > = {
    bigquery: {
      integer: "INT64",
      bigint: "INT64",
      float: "FLOAT64",
      decimal: "NUMERIC",
      number: "NUMERIC",
    },
    postgresql: {
      integer: "INT",
      bigint: "BIGINT",
      float: "DOUBLE PRECISION",
      decimal: "NUMERIC(38, 6)",
      number: "NUMERIC",
    },
    redshift: {
      integer: "INT",
      bigint: "BIGINT",
      float: "DOUBLE PRECISION",
      decimal: "NUMERIC(38, 6)",
      number: "NUMERIC",
    },
    mysql: {
      integer: "INT",
      bigint: "BIGINT",
      float: "DOUBLE",
      decimal: "DECIMAL(38, 6)",
      number: "DECIMAL(38, 6)",
    },
    mssql: {
      integer: "INT",
      bigint: "BIGINT",
      float: "FLOAT",
      decimal: "DECIMAL(38, 6)",
      number: "DECIMAL(38, 6)",
    },
  };

  const target = logical.toLowerCase() as LogicalCastType;
  const mapping = DIALECT_TYPE_MAP[dialect];

  if (!mapping) {
    throw new Error(
      `Unsupported dialect for logical cast type mapping: ${dialect}`,
    );
  }

  const resolved = mapping[target];
  if (!resolved) {
    throw new Error(`Unsupported logical cast type for ${dialect}: ${logical}`);
  }

  return resolved;
}
