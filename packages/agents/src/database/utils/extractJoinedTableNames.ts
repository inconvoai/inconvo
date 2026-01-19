import type { JoinPathHop } from "@repo/types";

interface JoinDescriptor {
  table: string;
  path: JoinPathHop[];
  name?: string;
  joinType?: string;
}

/**
 * Extracts table name from a fully qualified column reference (table.column format).
 */
function extractTableFromQualifiedColumn(
  qualifiedColumn: string,
): string | null {
  const lastDot = qualifiedColumn.lastIndexOf(".");
  if (lastDot === -1) return null;
  return qualifiedColumn.slice(0, lastDot);
}

/**
 * Extracts unique table names from an array of join descriptors.
 * Used to identify which tables are being joined so we can extend
 * table conditions for row-level security on those tables.
 *
 * This includes:
 * - The final target table from each join (join.table)
 * - All intermediate tables from join path hops (source and target columns)
 */
export function extractJoinedTableNames(
  joins: JoinDescriptor[] | null | undefined,
): string[] {
  if (!joins || joins.length === 0) {
    return [];
  }

  const tableNames = new Set<string>();

  for (const join of joins) {
    // Add the final target table
    tableNames.add(join.table);

    // Extract tables from all hops in the path (for intermediate tables)
    for (const hop of join.path) {
      // Extract from source columns
      for (const sourceCol of hop.source) {
        const tableName = extractTableFromQualifiedColumn(sourceCol);
        if (tableName) {
          tableNames.add(tableName);
        }
      }

      // Extract from target columns
      for (const targetCol of hop.target) {
        const tableName = extractTableFromQualifiedColumn(targetCol);
        if (tableName) {
          tableNames.add(tableName);
        }
      }
    }
  }

  return Array.from(tableNames);
}
