/**
 * Shared SQL assertion helpers for multi-schema / multi-dataset tests.
 *
 * Each dialect quotes identifiers differently (double-quotes for PG/MSSQL,
 * backticks for MySQL/BigQuery, brackets for MSSQL). This helper uses a
 * single regex that accepts any of these quoting styles so every dialect
 * test can share the same assertion function.
 */

const escapeRegex = (value: string) =>
  value.replace(/[.*+?^${}()|[\]\\]/g, "\\$&");

/**
 * Returns `true` when `sqlStr` contains a schema-qualified (or
 * dataset-qualified) reference to `schema.table`, regardless of identifier
 * quoting style.
 *
 * Handles: unquoted, double-quoted, backtick-quoted, and bracket-quoted
 * identifiers with optional whitespace around the dot.
 */
export function containsSchemaQualifiedTable(
  sqlStr: string,
  schema: string,
  table: string,
): boolean {
  const s = escapeRegex(schema);
  const t = escapeRegex(table);
  const pattern =
    "(?:`|\"|\\[)?" +
    s +
    "(?:`|\"|\\])?" +
    "\\s*\\.\\s*" +
    "(?:`|\"|\\[)?" +
    t +
    "(?:`|\"|\\])?";
  return new RegExp(pattern, "i").test(sqlStr);
}
