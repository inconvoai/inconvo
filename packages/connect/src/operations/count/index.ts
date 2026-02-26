import { Kysely, sql } from "kysely";
import type { Query } from "../../types/querySchema";
import type { OperationContext } from "../types";
import { buildWhereConditions } from "../utils/whereConditionBuilder";
import { getColumnFromTable } from "../utils/computedColumns";
import { getSchemaBoundDb } from "../utils/schemaHelpers";
import { resolveBaseSource } from "../utils/logicalTableSource";
import assert from "assert";
import {
  applyJoinHop,
  resolveJoinDescriptor,
  type QualifiedColumn,
} from "../utils/joinDescriptorHelpers";
import { executeWithLogging } from "../utils/executeWithLogging";
import { buildJsonObject } from "../utils/jsonBuilderHelpers";

export async function count(
  db: Kysely<any>,
  query: Query,
  ctx: OperationContext,
) {
  assert(query.operation === "count", "Invalid operation");
  const { table, whereAndArray, operationParameters } = query;
  const { count: countColumns, countDistinct, joins } = operationParameters;
  const { schema, dialect } = ctx;

  const dbForQuery = getSchemaBoundDb(db, schema, dialect);

  // Build query with schema-qualified table name
  const { source: baseSource } = resolveBaseSource({
    tableName: table,
    tableSchema: query.tableSchema ?? null,
    schema,
    dialect,
  });
  let dbQuery = dbForQuery.selectFrom(baseSource as any);

  const resolvedJoins =
    (joins ?? []).map((join) =>
      resolveJoinDescriptor({
        alias: join.name ?? join.table,
        tableName: join.table,
        joinType: join.joinType,
        path: join.path,
      }),
    ) ?? [];

  const aliasToTable = new Map<string, string>();
  aliasToTable.set(table, table);
  resolvedJoins.forEach((joinDescriptor) => {
    aliasToTable.set(joinDescriptor.alias, joinDescriptor.tableName);
    aliasToTable.set(joinDescriptor.tableName, joinDescriptor.tableName);
  });

  // Deduplicate hops to avoid duplicate table joins (MS SQL error 1013)
  const appliedHops = new Set<string>();
  for (const joinDescriptor of resolvedJoins) {
    for (const hop of joinDescriptor.hops) {
      // Create a unique key for this hop based on source and target columns
      const sourceKey = hop.source
        .map((c: QualifiedColumn) => `${c.tableName}.${c.columnName}`)
        .sort()
        .join(",");
      const targetKey = hop.target
        .map((c: QualifiedColumn) => `${c.tableName}.${c.columnName}`)
        .sort()
        .join(",");
      const hopKey = `${sourceKey}|${targetKey}`;
      if (appliedHops.has(hopKey)) {
        continue; // Skip duplicate hop
      }
      appliedHops.add(hopKey);
      dbQuery = applyJoinHop(dbQuery, joinDescriptor.joinType, hop, schema, dialect);
    }
  }

  const resolveColumnRef = (columnName: string) => {
    const { alias, column } = splitColumnReference(columnName);
    const targetAlias = alias;
    if (!targetAlias) {
      throw new Error(
        `Column ${columnName} must be qualified as tableOrAlias.column`,
      );
    }
    const targetTable = aliasToTable.get(targetAlias);
    if (!targetTable) {
      throw new Error(
        `Join alias ${targetAlias} not found for column ${columnName}`,
      );
    }
    return getColumnFromTable({
      columnName: column,
      tableName: targetTable,
      schema,
      dialect,
    });
  };

  const normalizedCountColumns = countColumns ?? [];
  const normalizedDistinctColumns = countDistinct ?? [];

  assert(
    normalizedCountColumns.length > 0 || normalizedDistinctColumns.length > 0,
    "Count operation requires at least one metric",
  );

  if (dialect === "mssql") {
    const selections: any[] = [];
    for (const columnName of normalizedCountColumns) {
      if (columnName === "_all") {
        selections.push(sql`COUNT(*)`.as(columnName));
      } else {
        const columnRef = resolveColumnRef(columnName);
        selections.push(sql`COUNT(${columnRef})`.as(columnName));
      }
    }
    for (const columnName of normalizedDistinctColumns) {
      const columnRef = resolveColumnRef(columnName);
      selections.push(
        sql`COUNT(DISTINCT ${columnRef})`.as(`distinct_${columnName}`),
      );
    }
    if (selections.length > 0) {
      dbQuery = dbQuery.select(selections);
    }
  } else {
    const countFields: [string, any][] = [];
    for (const columnName of normalizedCountColumns) {
      if (columnName === "_all") {
        countFields.push([columnName, sql`COUNT(*)`]);
      } else {
        const columnRef = resolveColumnRef(columnName);
        countFields.push([columnName, sql`COUNT(${columnRef})`]);
      }
    }

    const distinctFields: [string, any][] = [];
    for (const columnName of normalizedDistinctColumns) {
      const columnRef = resolveColumnRef(columnName);
      distinctFields.push([columnName, sql`COUNT(DISTINCT ${columnRef})`]);
    }

    const selectExpressions: any[] = [];
    if (countFields.length > 0) {
      selectExpressions.push(buildJsonObject(countFields, dialect).as("_count"));
    }
    if (distinctFields.length > 0) {
      selectExpressions.push(
        buildJsonObject(distinctFields, dialect).as("_countDistinct"),
      );
    }
    if (selectExpressions.length > 0) {
      dbQuery = dbQuery.select(selectExpressions);
    }
  }

  const whereCondition = buildWhereConditions(whereAndArray, table, schema, dialect, query.tableConditions);
  if (whereCondition) {
    dbQuery = dbQuery.where(whereCondition);
  }

  const { rows: result, compiled } = await executeWithLogging(dbQuery, {
    operation: "count",
  });

  let data: any;
  if (dialect === "mssql") {
    const counts: Record<string, number> = {};
    const distinctCounts: Record<string, number> = {};
    const firstRow = result[0] as Record<string, number> | undefined;
    if (firstRow) {
      for (const [key, value] of Object.entries(firstRow)) {
        if (key.startsWith("distinct_")) {
          distinctCounts[key.replace("distinct_", "")] = value;
        } else {
          counts[key] = value;
        }
      }
    }
    data = {
      _count: counts,
      _countDistinct:
        Object.keys(distinctCounts).length > 0 ? distinctCounts : undefined,
    };
  } else {
    const row = result[0] as Record<string, unknown> | undefined;
    if (row) {
      const parsedCount =
        typeof row._count === "string"
          ? JSON.parse(row._count)
          : (row._count as Record<string, number> | undefined);
      const parsedDistinct =
        typeof row._countDistinct === "string"
          ? JSON.parse(row._countDistinct)
          : (row._countDistinct as Record<string, number> | undefined);
      data = {
        _count: parsedCount ?? {},
        _countDistinct: parsedDistinct ?? undefined,
      };
    } else {
      data = { _count: {}, _countDistinct: undefined };
    }
  }

  return {
    query: { sql: compiled.sql, params: compiled.parameters },
    data,
  };
}

function splitColumnReference(columnName: string) {
  const lastDot = columnName.lastIndexOf(".");
  if (lastDot === -1) {
    throw new Error(
      `Column ${columnName} must be qualified as tableOrAlias.column`,
    );
  }
  const alias = columnName.slice(0, lastDot);
  const column = columnName.slice(lastDot + 1);
  if (!alias || !column) {
    throw new Error(`Invalid column reference: ${columnName}`);
  }
  return { alias, column };
}
