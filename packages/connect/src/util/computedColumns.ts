import type { SchemaResponse } from "~/types/types";
import type { ComputedColumn } from "~/types/querySchema";

export function getSchemaComputedColumns(
  schema: SchemaResponse
): ComputedColumn[] | undefined {
  const computed: ComputedColumn[] = [];
  for (const table of schema.tables) {
    if (!table.computedColumns?.length) {
      continue;
    }
    for (const column of table.computedColumns) {
      computed.push({
        name: column.name,
        table: { name: table.name },
        ast: column.ast,
      });
    }
  }
  return computed.length > 0 ? computed : undefined;
}
