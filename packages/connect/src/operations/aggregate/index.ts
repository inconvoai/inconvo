import { Kysely, sql } from "kysely";
import type { Query } from "../../types/querySchema";
import { buildWhereConditions } from "../utils/whereConditionBuilder";
import { getColumnFromTable } from "../utils/computedColumns";
import { getAugmentedSchema } from "../../util/augmentedSchemaCache";
import { buildJsonObject } from "../utils/jsonBuilderHelpers";
import { env } from "../../env";
import assert from "assert";
import {
  applyJoinHop,
  resolveJoinDescriptor,
} from "../utils/joinDescriptorHelpers";
import { parseJsonStrings, flattenObjectKeys } from "../utils/jsonParsing";
import { executeWithLogging } from "../utils/executeWithLogging";

export async function aggregate(db: Kysely<any>, query: Query) {
  assert(query.operation === "aggregate", "Invalid operation");
  const { table, whereAndArray, operationParameters } = query;

  const schema = await getAugmentedSchema();
  const aliasToTable = buildAliasToTable(table, operationParameters.joins);

  const createAggregateFields = (
    columns: string[] | undefined,
    aggregateFn: (column: any) => any,
  ): [string, any][] | undefined => {
    return columns?.map((columnName) => {
      const column = resolveColumnReference({
        columnName,
        baseTable: table,
        schema,
        aliasToTable,
      });
      return [columnName, aggregateFn(column)];
    });
  };

  const selectFields: any[] = [];

  const avgFields = createAggregateFields(
    operationParameters.avg ?? undefined,
    (column) => sql`AVG(${column})`,
  );
  const sumFields = createAggregateFields(
    operationParameters.sum ?? undefined,
    (column) => sql`SUM(${column})`,
  );
  const minFields = createAggregateFields(
    operationParameters.min ?? undefined,
    (column) => sql`MIN(${column})`,
  );
  const maxFields = createAggregateFields(
    operationParameters.max ?? undefined,
    (column) => sql`MAX(${column})`,
  );
  const countFields = createAggregateFields(
    operationParameters.count ?? undefined,
    (column) => sql`COUNT(${column})`,
  );
  const countDistinctFields = createAggregateFields(
    operationParameters.countDistinct ?? undefined,
    (column) => sql`COUNT(DISTINCT ${column})`,
  );
  const medianFields = createAggregateFields(
    operationParameters.median ?? undefined,
    (column) => buildMedianExpression(column),
  );

  if (avgFields) {
    selectFields.push(buildJsonObject(avgFields).as("_avg"));
  }
  if (sumFields) {
    selectFields.push(buildJsonObject(sumFields).as("_sum"));
  }
  if (minFields) {
    selectFields.push(buildJsonObject(minFields).as("_min"));
  }
  if (maxFields) {
    selectFields.push(buildJsonObject(maxFields).as("_max"));
  }
  if (countFields) {
    selectFields.push(buildJsonObject(countFields).as("_count"));
  }
  if (countDistinctFields) {
    selectFields.push(
      buildJsonObject(countDistinctFields).as("_countDistinct"),
    );
  }
  if (medianFields) {
    selectFields.push(buildJsonObject(medianFields).as("_median"));
  }

  let dbQuery = db.selectFrom(table);

  const resolvedJoins =
    (operationParameters.joins ?? []).map((join) =>
      resolveJoinDescriptor({
        alias: join.name ?? join.table,
        tableName: join.table,
        joinType: join.joinType,
        path: join.path,
      }),
    ) ?? [];

  for (const joinDescriptor of resolvedJoins) {
    for (const hop of joinDescriptor.hops) {
      dbQuery = applyJoinHop(dbQuery, joinDescriptor.joinType, hop);
    }
  }

  if (selectFields.length > 0) {
    dbQuery = dbQuery.select(selectFields);
  }

  const whereCondition = buildWhereConditions(whereAndArray, table, schema);
  if (whereCondition) {
    dbQuery = dbQuery.where(whereCondition);
  }

  const { rows: result, compiled } = await executeWithLogging(dbQuery, {
    operation: "aggregate",
  });
  const firstRow = result[0] as Record<string, unknown> | undefined;

  const parsedRow = firstRow
    ? (parseJsonStrings(firstRow) as Record<string, unknown>)
    : undefined;

  if (parsedRow) {
    for (const key of [
      "_avg",
      "_sum",
      "_min",
      "_max",
      "_count",
      "_countDistinct",
      "_median",
    ]) {
      const value = parsedRow[key];
      if (value && typeof value === "object" && !Array.isArray(value)) {
        parsedRow[key] = flattenObjectKeys(value as Record<string, unknown>);
      }
    }
  }

  return {
    query: { sql: compiled.sql, params: compiled.parameters },
    data: parsedRow,
  };
}

type AggregateQueryType = Extract<Query, { operation: "aggregate" }>;

function buildAliasToTable(
  baseTable: string,
  joins: AggregateQueryType["operationParameters"]["joins"],
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

function resolveColumnReference({
  columnName,
  baseTable: _baseTable,
  schema,
  aliasToTable,
}: {
  columnName: string;
  baseTable: string;
  schema: import("../../types/types").SchemaResponse;
  aliasToTable: Map<string, string>;
}) {
  const { alias, column } = splitColumnReference(columnName);
  const targetTable = aliasToTable.get(alias);
  if (!targetTable) {
    throw new Error(`Join alias ${alias} not found for column ${columnName}`);
  }

  return getColumnFromTable({
    columnName: column,
    tableName: targetTable,
    schema,
  });
}

function buildMedianExpression(column: any) {
  if (env.DATABASE_DIALECT === "bigquery") {
    return sql`APPROX_QUANTILES(${column}, 2)[OFFSET(1)]`;
  }

  return sql`cast(percentile_cont(0.5) within group (order by ${column}) as Numeric)`;
}
