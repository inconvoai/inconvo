import type { Schema } from "@repo/types";
import { type SQLComputedColumnAst, isActiveEnumColumn } from "@repo/types";

type TableSchema = Schema[number];

function buildEnumSummary(
  valueEnum: TableSchema["columns"][number]["valueEnum"],
) {
  if (!isActiveEnumColumn(valueEnum)) {
    return "";
  }
  const entries = valueEnum.entries.filter((entry) => entry.selected !== false);
  return entries.map((entry) => entry.label).join(", ");
}

export function stringifyComputedColumnAst(
  columnAst: SQLComputedColumnAst,
): string {
  switch (columnAst.type) {
    case "column":
      return columnAst.name;
    case "value":
      return String(columnAst.value);
    case "function": {
      const [arg] = columnAst.arguments;
      return `${columnAst.name}(${stringifyComputedColumnAst(arg)})`;
    }
    case "operation": {
      if (columnAst.operands.length === 0) {
        return "";
      }
      const operandStrings = columnAst.operands.map((operand) =>
        stringifyComputedColumnAst(operand),
      );
      const combined = operandStrings.join(` ${columnAst.operator} `);
      return `(${combined})`;
    }
    case "brackets":
      return `(${stringifyComputedColumnAst(columnAst.expression)})`;
    default: {
      const _exhaustiveCheck: never = columnAst;
      return _exhaustiveCheck;
    }
  }
}

export function buildTableSchemaStringFromTableSchema(
  tableSchema: TableSchema,
) {
  // Include schema prefix when present (e.g., "public.users" or "sales.orders")
  const tableName = tableSchema.schema
    ? `${tableSchema.schema}.${tableSchema.name}`
    : tableSchema.name;

  const access =
    tableSchema.access === "QUERYABLE"
      ? `Access: Selectable`
      : `Access: Can only be used when joined in from another table`;

  const columns = tableSchema.columns
    .map((column) => {
      const unitSuffix = column.unit ? ` [${column.unit}]` : "";
      const displayName = column.rename?.trim() ? column.rename : column.name;
      const notesSuffix = column.notes ? ` - Notes: ${column.notes}` : "";
      const hasConversion =
        Boolean(column.conversion?.selected) &&
        column.conversion?.type &&
        column.conversion.type !== column.type;
      const typeLabel = hasConversion
        ? `${column.effectiveType ?? column.type} (cast from ${column.type})`
        : (column.effectiveType ?? column.type);
      const enumSummary = column.valueEnum
        ? buildEnumSummary(column.valueEnum)
        : "";
      const enumSuffix = enumSummary ? ` - Allowed values: ${enumSummary}` : "";
      return `\t\t- ${displayName} (${typeLabel}${unitSuffix})${notesSuffix}${enumSuffix}`;
    })
    .join("\n")
    .replace(/^/, `\tColumns:\n`);
  const computedColumns = tableSchema.computedColumns
    .map((column, index) => {
      const unitSuffix = column.unit ? ` [${column.unit}]` : "";
      const notesSuffix = column.notes ? ` - Notes: ${column.notes}` : "";
      return `${index === 0 ? "\n" : ""}\t\t- ${column.name} (${
        column.type
      }${unitSuffix}) Alias for: ${stringifyComputedColumnAst(
        column.ast as SQLComputedColumnAst,
      )}${notesSuffix}`;
    })
    .join("\n");
  const relations =
    tableSchema.outwardRelations.length > 0
      ? tableSchema.outwardRelations
          .map((relation) => {
            // Include target schema for cross-schema relations
            const targetDisplay = relation.targetSchema
              ? `${relation.targetSchema}.${relation.targetTable.name}`
              : relation.targetTable.name;
            return `\t\t- ${relation.name} (${targetDisplay}${
              relation.isList ? "[]" : ""
            })`;
          })
          .join("\n")
          .replace(/^/, `\n\tRelations:\n`)
      : "";
  const context = tableSchema.context
    ? `\n\t<${tableSchema.name}TableContext>\n${tableSchema.context}\n\t</${tableSchema.name}TableContext>`
    : "";

  return `${tableName}\n\t${access}\n${columns}${computedColumns}${relations}${context}`;
}

function buildTableSchemaString(schema: Schema, table: string) {
  const tableSchema = schema.find((t) => t.name === table);
  if (!tableSchema) {
    return "";
  }
  return buildTableSchemaStringFromTableSchema(tableSchema);
}

export function buildFullSchemaString(schema: Schema): string {
  return schema
    .map((table) => {
      return buildTableSchemaStringFromTableSchema(table);
    })
    .join("\n\n");
}

export function buildRelatedTablesSchemaString(
  schema: Schema,
  table: string,
): string {
  const queue = [table];
  const visited = new Set<string>();
  let relatedTableSchema = "";

  while (queue.length > 0) {
    const currentTable = queue.shift();
    if (!currentTable || visited.has(currentTable)) {
      continue;
    }
    visited.add(currentTable);
    const tableSchemaString = buildTableSchemaString(schema, currentTable);
    if (!tableSchemaString) {
      continue;
    }
    relatedTableSchema += `${currentTable}\n${tableSchemaString}\n`;

    const currentTableSchema = schema.find((t) => t.name === currentTable);
    if (!currentTableSchema) {
      continue;
    }

    const relatedTables = currentTableSchema.columns
      .flatMap((column) => {
        const relations = Array.isArray(column.relation) ? column.relation : [];
        return relations
          .map((rel) => rel.relation?.targetTable?.name)
          .filter((name): name is string => Boolean(name));
      })
      .filter(
        (relatedTable): relatedTable is string => relatedTable !== undefined,
      );

    queue.push(...relatedTables);
  }

  return relatedTableSchema;
}
