import { sql, type RawBuilder } from "kysely";
import {
  type LogicalCastType,
  SQLCastExpressionAst,
  SQLComputedColumnAst,
} from "~/types/querySchema";
import { env } from "~/env";
import type { SchemaResponse } from "~/types/types";

export function getColumnFromTable({
  columnName,
  tableName,
  schema,
}: {
  columnName: string;
  tableName: string;
  schema: SchemaResponse;
}): RawBuilder<unknown> {
  const table = schema.tables.find((t) => t.name === tableName);
  if (!table) {
    throw new Error(`Table not found in schema: ${tableName}`);
  }

  // Check if it's a computed column
  const computedColumn = table.computedColumns?.find(
    (cc) => cc.name === columnName,
  );

  if (computedColumn) {
    return generateComputedColumnAsSQL(
      computedColumn.ast,
      tableName,
      schema,
      false,
    );
  }

  const columnConversion = table.columnConversions?.find(
    (conversion) => conversion.column === columnName,
  );

  if (columnConversion) {
    return generateColumnConversionAsSQL(columnConversion.ast, tableName);
  }

  return buildColumnReference(tableName, columnName);
}

function buildColumnReference(
  tableName: string,
  columnName: string,
): RawBuilder<unknown> {
  // BigQuery STRUCT field handling: columns with # separator represent nested STRUCT fields
  // Column name format: "structColumn#fieldPath" or "structColumn#nested#field"
  // Converts to BigQuery SQL: table.structColumn.fieldPath or table.structColumn.nested.field
  if (env.DATABASE_DIALECT === "bigquery" && columnName.includes("#")) {
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
): RawBuilder<unknown> {
  switch (ast.type) {
    case "column":
      return buildColumnReference(tableName, ast.name);
    case "value":
      return sql`${ast.value}`;
    case "cast": {
      const inner = generateColumnConversionAsSQL(ast.expression, tableName);
      const castKeyword =
        env.DATABASE_DIALECT === "bigquery" ? "SAFE_CAST" : "CAST";
      const target = resolveLogicalType(ast.as);
      return sql`${sql.raw(castKeyword)}(${inner} AS ${sql.raw(target)})`;
    }
    case "coalesce": {
      const expression = generateColumnConversionAsSQL(
        ast.expression,
        tableName,
      );
      const fallback = generateColumnConversionAsSQL(ast.fallback, tableName);
      return sql`COALESCE(${expression}, ${fallback})`;
    }
    case "brackets": {
      const inner = generateColumnConversionAsSQL(ast.expression, tableName);
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
  tableName: string,
  schema: SchemaResponse,
  numericRequired: boolean,
): RawBuilder<unknown> {
  switch (ast.type) {
    case "column":
      // If a conversion exists, use it; otherwise enforce numeric when required
      const table = schema.tables.find((t) => t.name === tableName);
      const conversion = table?.columnConversions?.find(
        (c) => c.column === ast.name,
      );
      if (conversion) {
        return generateColumnConversionAsSQL(conversion.ast, tableName);
      }
      if (numericRequired) {
        const columnType = table?.columns.find(
          (col) => col.name === ast.name,
        )?.type;
        if (columnType && columnType.toLowerCase() !== "number") {
          throw new Error(
            `Computed column requires numeric input but '${ast.name}' is type '${columnType}' without a conversion`,
          );
        }
      }
      return sql`${sql.ref(tableName)}.${sql.ref(ast.name)}`;

    case "value":
      return sql`${ast.value}`;

    case "operation": {
      const operands = ast.operands.map((operand) =>
        generateComputedColumnAsSQL(operand, tableName, schema, true),
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

    case "brackets":
      const inner = generateComputedColumnAsSQL(
        ast.expression,
        tableName,
        schema,
        numericRequired,
      );
      return sql`(${inner})`;

    default:
      throw new Error(`Unknown AST node type: ${(ast as any).type}`);
  }
}

function resolveLogicalType(logical: LogicalCastType): string {
  const DIALECT_TYPE_MAP: Record<
    typeof env.DATABASE_DIALECT,
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

  const dialect = env.DATABASE_DIALECT;
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
