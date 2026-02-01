import { Kysely } from "kysely";
import type { Query } from "../../types/querySchema";
import type { OperationContext } from "../types";
import { buildWhereConditions } from "../utils/whereConditionBuilder";
import { getColumnFromTable } from "../utils/computedColumns";
import { applyLimit } from "../utils/queryHelpers";
import { getTableIdentifier } from "../utils/tableIdentifier";
import assert from "assert";
import { resolveJoinDescriptor } from "../utils/joinDescriptorHelpers";
import { executeWithLogging } from "../utils/executeWithLogging";

interface JoinInfo {
  tableName: string;
  alias: string; // SQL alias for the joined table (may differ from tableName for duplicate joins)
}

interface JoinEdge {
  sourceAlias: string; // SQL alias or identifier for the source side of the hop
  sourceCol: string;
  targetTable: string;
  targetCol: string;
  targetAlias: string; // SQL alias for the target table
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

  // Build join info map for selections and a join plan that tracks SQL aliases per hop.
  const joinInfoMap = new Map<string, JoinInfo>();
  const joinEdges: JoinEdge[] = [];
  const usedSqlAliases = new Set<string>([table]);
  const hopToSqlAlias = new Map<string, string>();
  const baseTableId = getTableIdentifier(table, query.tableSchema, dialect);
  const baseAlias = dialect === "bigquery" ? table : baseTableId;

  const allocateSqlAlias = (tableName: string) => {
    if (!usedSqlAliases.has(tableName)) {
      usedSqlAliases.add(tableName);
      return tableName;
    }
    let counter = 2;
    while (usedSqlAliases.has(`${tableName}_${counter}`)) {
      counter++;
    }
    const alias = `${tableName}_${counter}`;
    usedSqlAliases.add(alias);
    return alias;
  };

  for (const join of resolvedJoins) {
    let sourceAlias = baseAlias;
    let lastTargetTable: string | null = null;

    for (const hop of join.hops) {
      if (!hop || hop.source.length === 0 || hop.target.length === 0) continue;

      const sourceCol = hop.source[0]?.columnName;
      const targetTable = hop.target[0]?.tableName;
      const targetCol = hop.target[0]?.columnName;

      if (!sourceCol || !targetTable || !targetCol) continue;

      // Include the current SQL alias in the hop key to avoid misbinding multi-hop joins.
      const hopKey = `${sourceAlias}.${sourceCol}|${targetTable}.${targetCol}`;
      let targetAlias = hopToSqlAlias.get(hopKey);

      if (!targetAlias) {
        targetAlias = allocateSqlAlias(targetTable);
        hopToSqlAlias.set(hopKey, targetAlias);
        joinEdges.push({
          sourceAlias,
          sourceCol,
          targetTable,
          targetCol,
          targetAlias,
        });
      }

      sourceAlias = targetAlias;
      lastTargetTable = targetTable;
    }

    if (lastTargetTable) {
      joinInfoMap.set(join.alias, {
        tableName: lastTargetTable,
        alias: sourceAlias,
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

  // Build query with schema-qualified table name
  const tableId = baseTableId;
  let dbQuery: any = db.selectFrom(tableId);

  // Add JOINs - use SQL alias when joining same table multiple times
  // Track which SQL aliases we've already created JOINs for to avoid duplicates
  const createdJoins = new Set<string>();
  for (const joinEdge of joinEdges) {
    if (createdJoins.has(joinEdge.targetAlias)) continue;
    createdJoins.add(joinEdge.targetAlias);

    const joinedTable = schema.tables.find((t) => t.name === joinEdge.targetTable);
    const joinedTableId = getTableIdentifier(
      joinEdge.targetTable,
      joinedTable?.schema,
      dialect,
    );

    const joinTarget =
      joinEdge.targetAlias !== joinEdge.targetTable
        ? `${joinedTableId} as ${joinEdge.targetAlias}`
        : joinedTableId;

    dbQuery = dbQuery.leftJoin(joinTarget, (join: any) =>
      join.onRef(
        `${joinEdge.sourceAlias}.${joinEdge.sourceCol}`,
        "=",
        `${joinEdge.targetAlias}.${joinEdge.targetCol}`,
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
