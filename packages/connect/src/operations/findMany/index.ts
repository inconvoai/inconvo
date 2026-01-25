import { Kysely } from "kysely";
import type { Query } from "../../types/querySchema";
import type { OperationContext } from "../types";
import { buildWhereConditions } from "../utils/whereConditionBuilder";
import { getColumnFromTable } from "../utils/computedColumns";
import { applyLimit } from "../utils/queryHelpers";
import assert from "assert";
import { resolveJoinDescriptor } from "../utils/joinDescriptorHelpers";
import { executeWithLogging } from "../utils/executeWithLogging";

interface JoinInfo {
  tableName: string;
  alias: string; // SQL alias for the joined table (may differ from tableName for duplicate joins)
  sourceTable: string;
  sourceCol: string;
  targetCol: string;
}

interface ColumnInfo {
  table: string;
  column: string;
}

export async function findMany(
  db: Kysely<any>,
  query: Query,
  ctx: OperationContext,
) {
  assert(query.operation === "findMany", "Invalid operation");
  const { table, whereAndArray, operationParameters } = query;
  const { select, orderBy, limit } = operationParameters;
  const { schema, dialect } = ctx;

  const selectMap: Record<string, string[] | undefined> = { ...select };
  if (!selectMap[table]) {
    selectMap[table] = [];
  }

  // Resolve joins
  const resolvedJoins = (operationParameters.joins ?? []).map((join) =>
    resolveJoinDescriptor({
      alias: join.name ?? join.table,
      tableName: join.table,
      path: join.path,
    }),
  );

  // Build join info map
  // Track by alias to allow same table joined multiple times with different aliases
  const joinInfoMap = new Map<string, JoinInfo>();
  const usedSqlAliases = new Set<string>([table]);

  for (const join of resolvedJoins) {
    for (let i = 0; i < join.hops.length; i++) {
      const hop = join.hops[i];
      if (!hop || hop.source.length === 0 || hop.target.length === 0) continue;

      const sourceTable = hop.source[0]?.tableName;
      const sourceCol = hop.source[0]?.columnName;
      const targetTable = hop.target[0]?.tableName;
      const targetCol = hop.target[0]?.columnName;

      if (!sourceTable || !sourceCol || !targetTable || !targetCol) continue;

      const isLastHop = i === join.hops.length - 1;
      const aliasKey = isLastHop ? join.alias : targetTable;

      // Skip if we've already processed this exact alias
      if (joinInfoMap.has(aliasKey)) continue;

      // Determine SQL alias - use table name if unique, otherwise use the aliasKey
      // This allows joining the same table multiple times with different aliases
      let sqlAlias = targetTable;
      if (usedSqlAliases.has(targetTable)) {
        // Table already joined - need a unique SQL alias
        sqlAlias = aliasKey.replace(/\./g, "_");
      }
      usedSqlAliases.add(sqlAlias);

      joinInfoMap.set(aliasKey, {
        tableName: targetTable,
        alias: sqlAlias,
        sourceTable,
        sourceCol,
        targetCol,
      });
    }
  }

  // Build selections
  const selections: any[] = [];
  const columnAliasMap = new Map<string, ColumnInfo>();

  // Base table columns
  for (const col of selectMap[table] ?? []) {
    const colRef = getColumnFromTable({
      columnName: col,
      tableName: table,
      schema,
      dialect,
    });
    const alias = `${table}__${col}`;
    selections.push(colRef.as(alias));
    columnAliasMap.set(alias, { table, column: col });
  }

  // Joined table columns - use SQL alias for column references
  for (const [alias, joinInfo] of joinInfoMap) {
    for (const col of selectMap[alias] ?? []) {
      // Get column reference using the SQL alias (not the original table name)
      const colRef = getColumnFromTable({
        columnName: col,
        tableName: joinInfo.tableName,
        schema,
        dialect,
        tableAlias: joinInfo.alias, // Use SQL alias for the column reference
      });
      const colAlias = `${alias.replace(/\./g, "__")}__${col}`;
      selections.push(colRef.as(colAlias));
      columnAliasMap.set(colAlias, { table: alias, column: col });
    }
  }

  // Build query
  let dbQuery: any = db.selectFrom(table);

  // Add JOINs - use SQL alias when joining same table multiple times
  for (const [, joinInfo] of joinInfoMap) {
    // If alias differs from table name, use "table AS alias" syntax
    const joinTarget =
      joinInfo.alias !== joinInfo.tableName
        ? `${joinInfo.tableName} as ${joinInfo.alias}`
        : joinInfo.tableName;

    dbQuery = dbQuery.leftJoin(joinTarget, (join: any) =>
      join.onRef(
        `${joinInfo.sourceTable}.${joinInfo.sourceCol}`,
        "=",
        `${joinInfo.alias}.${joinInfo.targetCol}`,
      ),
    );
  }

  dbQuery = dbQuery.select(selections);

  // WHERE
  const whereCondition = buildWhereConditions(
    whereAndArray,
    table,
    schema,
    dialect,
    query.tableConditions,
  );
  if (whereCondition) {
    dbQuery = dbQuery.where(whereCondition);
  }

  // ORDER BY
  if (orderBy) {
    const columnRef = getColumnFromTable({
      columnName: orderBy.column,
      tableName: table,
      schema,
      dialect,
    });
    dbQuery = dbQuery.orderBy(columnRef, orderBy.direction ?? "asc");
  }

  // LIMIT
  dbQuery = applyLimit(dbQuery, limit, dialect);

  // Execute
  const { rows: rawRows, compiled } = await executeWithLogging(dbQuery, {
    operation: "findMany",
  });

  // Transform column aliases to clean output format
  const processedRows = transformRows(rawRows, columnAliasMap);

  return {
    query: { sql: compiled.sql, params: compiled.parameters },
    data: processedRows,
  };
}

/**
 * Transform flat JOIN rows to clean output format.
 * Base table: "tablename_column"
 * Joined tables: "alias_column" (using the provided join alias)
 */
function transformRows(
  rows: any[],
  columnAliasMap: Map<string, ColumnInfo>,
): any[] {
  return rows.map((row) => {
    const output: Record<string, any> = {};
    for (const [alias, info] of columnAliasMap) {
      const value = row[alias];
      output[`${info.table}_${info.column}`] = value;
    }
    return output;
  });
}
