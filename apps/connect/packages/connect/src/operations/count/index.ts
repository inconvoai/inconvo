import { type Query } from "~/types/querySchema";
import {
  count as dCount,
  countDistinct as dDistinctCount,
  SQL,
} from "drizzle-orm";
import { parsePrismaWhere } from "~/operations/utils/prismaToDrizzleWhereConditions";
import { loadDrizzleSchema } from "~/util/loadDrizzleSchema";
import assert from "assert";
import { getColumnFromTable } from "../utils/getColumnFromTable";
import { buildJsonObjectSelect } from "../utils/jsonBuilderHelpers";
import {
  buildJoinCondition,
  normaliseJoinHop,
} from "../utils/joinDescriptorHelpers";

export async function count(db: any, query: Query) {
  assert(query.operation === "count", "Invalid inconvo operation");
  const { table, whereAndArray, operationParameters, computedColumns } = query;

  const drizzleSchema = await loadDrizzleSchema();

  const aliasToTable = new Map<string, string>();
  aliasToTable.set(table, table);

  const joins = operationParameters.joins ?? [];
  for (const join of joins) {
    const alias = join.name ?? join.table;
    aliasToTable.set(alias, join.table);
    aliasToTable.set(join.table, join.table);
  }

  const resolveColumn = (columnName: string) => {
    if (columnName === "_all") {
      return dCount();
    }

    const { alias: targetAlias, column } = splitColumnReference(columnName);
    const targetTable = aliasToTable.get(targetAlias);
    if (!targetTable) {
      throw new Error(`Join alias ${targetAlias} not found for column ${columnName}`);
    }

    const shouldUseComputed = targetTable === table;
    return dCount(
      getColumnFromTable({
        columnName: column,
        tableName: targetTable,
        drizzleSchema,
        computedColumns: shouldUseComputed ? computedColumns : undefined,
      })
    );
  };

  const resolveDistinctColumn = (columnName: string) => {
    const { alias: targetAlias, column } = splitColumnReference(columnName);
    const targetTable = aliasToTable.get(targetAlias);
    if (!targetTable) {
      throw new Error(`Join alias ${targetAlias} not found for column ${columnName}`);
    }

    const shouldUseComputed = targetTable === table;
    return dDistinctCount(
      getColumnFromTable({
        columnName: column,
        tableName: targetTable,
        drizzleSchema,
        computedColumns: shouldUseComputed ? computedColumns : undefined,
      })
    );
  };

  const countColumns: [string, SQL<unknown>][] = (operationParameters.count ??
    []
  ).map((columnName) => [columnName, resolveColumn(columnName)]);

  const distinctColumns: [string, SQL<unknown>][] = (
    operationParameters.countDistinct ?? []
  ).map((columnName) => [
    columnName,
    resolveDistinctColumn(columnName),
  ]);

  const selection: Record<string, SQL<unknown>> = {};
  if (countColumns.length > 0) {
    selection["_count"] = buildJsonObjectSelect(countColumns);
  }
  if (distinctColumns.length > 0) {
    selection["_countDistinct"] = buildJsonObjectSelect(distinctColumns);
  }

  assert(
    Object.keys(selection).length > 0,
    "Count operation requires at least one metric"
  );

  let dbQuery = db
    .select(selection)
    .from(drizzleSchema[table])
    .where((columns: Record<string, unknown>) =>
      parsePrismaWhere({
        drizzleSchema,
        tableName: table,
        where: whereAndArray,
        columns,
        computedColumns: computedColumns,
      })
    );

  for (const join of joins) {
    const joinType = join.joinType ?? "left";
    for (const hop of join.path) {
      const metadata = normaliseJoinHop(hop);
      const targetTableName = metadata.target[0]?.tableName;
      if (!targetTableName) {
        throw new Error("Join descriptor hop is missing target table metadata.");
      }
      const joinCondition = buildJoinCondition(
        metadata,
        drizzleSchema,
        computedColumns
      );

      switch (joinType) {
        case "inner":
          dbQuery = dbQuery.innerJoin(
            drizzleSchema[targetTableName],
            joinCondition
          );
          break;
        case "right":
          dbQuery = dbQuery.rightJoin(
            drizzleSchema[targetTableName],
            joinCondition
          );
          break;
        case "left":
        default:
          dbQuery = dbQuery.leftJoin(
            drizzleSchema[targetTableName],
            joinCondition
          );
          break;
      }
    }
  }

  const response = await dbQuery;

  const baseResult = (response[0] ?? {}) as Record<string, unknown>;
  const data =
    countColumns.length === 0 && baseResult._count === undefined
      ? { ...baseResult, _count: {} }
      : baseResult;

  return { query: dbQuery.toSQL(), data };
}

function splitColumnReference(columnName: string) {
  const lastDot = columnName.lastIndexOf(".");
  if (lastDot === -1) {
    throw new Error(
      `Column ${columnName} must be qualified as tableOrAlias.column`
    );
  }
  const alias = columnName.slice(0, lastDot);
  const column = columnName.slice(lastDot + 1);
  if (!alias || !column) {
    throw new Error(`Invalid column reference: ${columnName}`);
  }
  return { alias, column };
}
