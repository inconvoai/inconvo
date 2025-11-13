import { type Query } from "~/types/querySchema";
import { avg, min, sql, sum, max, count, SQL } from "drizzle-orm";
import { parsePrismaWhere } from "~/operations/utils/prismaToDrizzleWhereConditions";
import { loadDrizzleSchema } from "~/util/loadDrizzleSchema";
import { getColumnFromTable } from "../utils/getColumnFromTable";
import { buildJsonObjectSelect } from "../utils/jsonBuilderHelpers";
import assert from "assert";
import {
  buildJoinCondition,
  normaliseJoinHop,
} from "../utils/joinDescriptorHelpers";

export async function aggregate(db: any, query: Query) {
  assert(query.operation === "aggregate", "Invalid inconvo operation");
  const {
    table: tableName,
    whereAndArray,
    operationParameters,
    computedColumns,
  } = query;

  const drizzleSchema = await loadDrizzleSchema();
const aliasToTable = buildAliasToTable(tableName, operationParameters.joins);

  const createAggregateFields = (
    columns: string[] | undefined,
    aggregateFn: (column: SQL<any>) => SQL<any>
  ): [string, SQL<any>][] | undefined => {
    return columns?.map((columnName) => {
      const column = resolveColumnReference({
        columnName,
        baseTable: tableName,
        drizzleSchema,
        computedColumns,
        aliasToTable,
      });
      return [columnName, aggregateFn(column)];
    });
  };

  const selectFields: Record<string, any> = {};

  const avgFields = createAggregateFields(
    operationParameters.avg ?? undefined,
    (column) => avg(column)
  );
  const sumFields = createAggregateFields(
    operationParameters.sum ?? undefined,
    (column) => sum(column)
  );
  const minFields = createAggregateFields(
    operationParameters.min ?? undefined,
    (column) => min(column)
  );
  const maxFields = createAggregateFields(
    operationParameters.max ?? undefined,
    (column) => max(column)
  );
  const countFields = createAggregateFields(
    operationParameters.count ?? undefined,
    (column) => count(column)
  );
  const medianFields = createAggregateFields(
    operationParameters.median ?? undefined,
    (column) =>
      sql<number>`cast(percentile_cont(0.5) within group (order by ${column}) as Numeric)`.mapWith(
        Number
      )
  );

  if (avgFields) {
    selectFields["_avg"] = buildJsonObjectSelect(avgFields);
  }
  if (sumFields) {
    selectFields["_sum"] = buildJsonObjectSelect(sumFields);
  }
  if (minFields) {
    selectFields["_min"] = buildJsonObjectSelect(minFields);
  }
  if (maxFields) {
    selectFields["_max"] = buildJsonObjectSelect(maxFields);
  }
  if (countFields) {
    selectFields["_count"] = buildJsonObjectSelect(countFields);
  }
  if (medianFields) {
    selectFields["_median"] = buildJsonObjectSelect(medianFields);
  }

  let dbQuery = db
    .select(selectFields)
    .from(drizzleSchema[tableName])
    .where((columns: Record<string, SQL>) =>
      parsePrismaWhere({
        drizzleSchema,
        columns,
        tableName,
        where: whereAndArray,
        computedColumns,
      })
    );

  const joins = operationParameters.joins ?? [];
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

  const sql = dbQuery.toSQL();
  const response = await dbQuery;
  return { query: sql, data: response[0] };
}

type AggregateQueryType = Extract<Query, { operation: "aggregate" }>;

function buildAliasToTable(
  baseTable: string,
  joins: AggregateQueryType["operationParameters"]["joins"]
) {
  const map = new Map<string, string>();
  map.set(baseTable, baseTable);

  joins?.forEach((join) => {
    const alias = join.name ?? join.table;
    map.set(alias, join.table);
    map.set(join.table, join.table);
  });

  return map;
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

function resolveColumnReference({
  columnName,
  baseTable,
  drizzleSchema,
  computedColumns,
  aliasToTable,
}: {
  columnName: string;
  baseTable: string;
  drizzleSchema: Record<string, any>;
  computedColumns: Query["computedColumns"];
  aliasToTable: Map<string, string>;
}) {
  const { alias, column } = splitColumnReference(columnName);
  const targetTable = aliasToTable.get(alias);
  if (!targetTable) {
    throw new Error(`Join alias ${alias} not found for column ${columnName}`);
  }

  const shouldUseComputed = targetTable === baseTable;

  return getColumnFromTable({
    columnName: column,
    tableName: targetTable,
    drizzleSchema,
    computedColumns: shouldUseComputed ? computedColumns : undefined,
  });
}
