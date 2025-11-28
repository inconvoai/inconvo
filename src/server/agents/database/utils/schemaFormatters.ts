import type { Schema } from "~/server/db/schema";
import { type SQLComputedColumnAst } from "~/server/userDatabaseConnector/types";

type TableSchema = Schema[number];

export function stringifyComputedColumnAst(
  columnAst: SQLComputedColumnAst
): string {
  switch (columnAst.type) {
    case "column":
      return columnAst.name;
    case "value":
      return String(columnAst.value);
    case "operation": {
      if (columnAst.operands.length === 0) {
        return "";
      }
      const operandStrings = columnAst.operands.map((operand) =>
        stringifyComputedColumnAst(operand)
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
  tableSchema: TableSchema
) {
  const access =
    tableSchema.access === "QUERYABLE"
      ? `Access: Selectable`
      : `Access: Can only be used when joined in from another table`;

  const columns = tableSchema.columns
    .map((column) => {
      const unitSuffix = column.unit ? ` [${column.unit}]` : "";
      const displayName = column.rename?.trim() ? column.rename : column.name;
      const notesSuffix = column.notes ? ` - Notes: ${column.notes}` : "";
      return `\t\t- ${displayName} (${column.type}${unitSuffix})${notesSuffix}`;
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
        column.ast as SQLComputedColumnAst
      )}${notesSuffix}`;
    })
    .join("\n");
  const relations =
    tableSchema.outwardRelations.length > 0
      ? tableSchema.outwardRelations
          .map((relation) => {
            return `\t\t- ${relation.name} (${relation.targetTable.name}${
              relation.isList ? "[]" : ""
            })`;
          })
          .join("\n")
          .replace(/^/, `\n\tRelations:\n`)
      : "";
  const context = tableSchema.context
    ? `\n\t<${tableSchema.name}TableContext>\n${tableSchema.context}\n\t</${tableSchema.name}TableContext>`
    : "";

  return `${tableSchema.name}\n\t${access}\n${columns}${computedColumns}${relations}${context}`;
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
  table: string
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
        const relations = Array.isArray(column.relation)
          ? column.relation
          : [];
        return relations
          .map((rel) => rel.relation?.targetTable?.name)
          .filter((name): name is string => Boolean(name));
      })
      .filter(
        (relatedTable): relatedTable is string => relatedTable !== undefined
      );

    queue.push(...relatedTables);
  }

  return relatedTableSchema;
}
