import assert from "assert";
import { Kysely, Expression, SqlBool, sql } from "kysely";
import { Query } from "~/types/querySchema";
import { buildWhereConditions } from "~/operations/utils/whereConditionBuilder";
import { getAugmentedSchema } from "~/util/augmentedSchemaCache";
import { getSchemaBoundDb } from "~/operations/utils/schemaHelpers";
import { createAggregationFields } from "~/operations/utils/createAggregationFields";
import { getColumnFromTable } from "~/operations/utils/computedColumns";
import { buildDateIntervalExpression } from "~/operations/utils/buildDateIntervalExpression";
import { buildDateComponentExpressions } from "~/operations/utils/buildDateComponentExpression";
import {
  applyJoinHop,
  normaliseJoinHop,
} from "~/operations/utils/joinDescriptorHelpers";
import { buildJsonObject } from "~/operations/utils/jsonBuilderHelpers";
import { parseJsonStrings } from "~/operations/utils/jsonParsing";
import { env } from "~/env";
import { buildAggregateExpression } from "~/operations/utils/aggregateExpressionBuilder";
import { applyHavingComparison } from "~/operations/utils/havingComparison";

type Reducer = "sum" | "min" | "max" | "avg";

export async function aggregateGroups(db: Kysely<any>, query: Query) {
  assert(query.operation === "aggregateGroups", "Invalid operation");
  const { table, whereAndArray, operationParameters } = query;
  const schema = await getAugmentedSchema();
  const dbForQuery = getSchemaBoundDb(db, schema);
  const { groupBy: groupByList, joins, having } = operationParameters;

  const aliasRenameMap = new Map<string, string>();
  const getDialectAlias = (alias: string) => {
    if (env.DATABASE_DIALECT === "bigquery") {
      let sanitized = alias.replace(/[^a-zA-Z0-9_]/g, "__");
      sanitized = sanitized.replace(/_{3,}/g, "__");
      aliasRenameMap.set(sanitized, alias);
      return sanitized;
    }
    return alias;
  };

  const groupByColumns: any[] = [];
  const groupKeyExpressions = new Map<string, { select: any; order: any }>();

  for (const key of groupByList) {
    if (key.type === "column") {
      assert(
        key.column.split(".").length === 2,
        "Invalid column format for group by (not table.column)"
      );
      const [tableName, columnName] = key.column.split(".");
      const column = getColumnFromTable({
        columnName,
        tableName,
        schema,
      });
      const alias = key.alias ?? `${tableName}.${columnName}`;
      const dialectAlias = getDialectAlias(alias);
      groupByColumns.push(column);
      groupKeyExpressions.set(alias, { select: sql`${column}`.as(dialectAlias), order: column });
    } else if (key.type === "dateInterval") {
      assert(
        key.column.split(".").length === 2,
        "Invalid column format for group by interval (not table.column)"
      );
      const [tableName, columnName] = key.column.split(".");
      const column = getColumnFromTable({
        columnName,
        tableName,
        schema,
      });
      const alias = key.alias ?? `${tableName}.${columnName}|${key.interval}`;
      const dialectAlias = getDialectAlias(alias);
      const intervalExpression = buildDateIntervalExpression(
        column,
        key.interval
      );
      groupByColumns.push(intervalExpression);
      groupKeyExpressions.set(alias, {
        select: intervalExpression.as(dialectAlias),
        order: intervalExpression,
      });
    } else if (key.type === "dateComponent") {
      assert(
        key.column.split(".").length === 2,
        "Invalid column format for group by component (not table.column)"
      );
      const [tableName, columnName] = key.column.split(".");
      const column = getColumnFromTable({
        columnName,
        tableName,
        schema,
      });
      const alias = key.alias ?? `${tableName}.${columnName}|${key.component}`;
      const dialectAlias = getDialectAlias(alias);
      const { select, order } = buildDateComponentExpressions(
        column,
        key.component
      );
      groupByColumns.push(select);
      groupByColumns.push(order);
      groupKeyExpressions.set(alias, { select: select.as(dialectAlias), order });
    }
  }

  const baseAggregations = {
    count: createAggregationFields(
      operationParameters.aggregates.count ?? undefined,
      (column) => sql`COUNT(${column})`,
      schema
    ),
    countDistinct: createAggregationFields(
      operationParameters.aggregates.countDistinct ?? undefined,
      (column) => sql`COUNT(DISTINCT ${column})`,
      schema
    ),
    min: createAggregationFields(
      operationParameters.aggregates.min ?? undefined,
      (column) => sql`MIN(${column})`,
      schema
    ),
    max: createAggregationFields(
      operationParameters.aggregates.max ?? undefined,
      (column) => sql`MAX(${column})`,
      schema
    ),
    sum: createAggregationFields(
      operationParameters.aggregates.sum ?? undefined,
      (column) => sql`SUM(${column})`,
      schema
    ),
    avg: createAggregationFields(
      operationParameters.aggregates.avg ?? undefined,
      (column) => sql`AVG(${column})`,
      schema
    ),
  };

  const perGroupSelections: any[] = [];
  const perGroupAliases = new Map<string, string>(); // key: family|column -> alias
  const mssqlJsonKeyMap = new Map<string, string>(); // original -> sanitized

  const getPerGroupAlias = (family: string, column: string) => {
    const safeColumn = column.replace(/[^a-zA-Z0-9_]/g, "__");
    const alias = getDialectAlias(`${family}__${safeColumn}`);
    perGroupAliases.set(`${family}|${column}`, alias);
    return alias;
  };

  const getJsonKeyForReducer = (column: string) => {
    if (env.DATABASE_DIALECT !== "mssql") return column;
    const existing = mssqlJsonKeyMap.get(column);
    if (existing) return existing;
    const sanitized = `json_${mssqlJsonKeyMap.size}`;
    mssqlJsonKeyMap.set(column, sanitized);
    aliasRenameMap.set(sanitized, column);
    return sanitized;
  };

  for (const [family, fields] of Object.entries(baseAggregations)) {
    if (!fields) continue;
    for (const [column, expr] of fields) {
      perGroupSelections.push(expr.as(getPerGroupAlias(family, column)));
    }
  }

  let groupQuery = dbForQuery.selectFrom(table);

  if (joins && joins.length > 0) {
    for (const join of joins) {
      const joinType = join.joinType ?? "left";
      for (const hop of join.path) {
        const metadata = normaliseJoinHop(hop);
        groupQuery = applyJoinHop(groupQuery, joinType, metadata);
      }
    }
  }

  const whereExpr = buildWhereConditions(whereAndArray, table, schema);
  if (whereExpr) {
    groupQuery = groupQuery.where(whereExpr);
  }

  if (perGroupSelections.length > 0) {
    groupQuery = groupQuery.select(perGroupSelections);
  } else {
    groupQuery = groupQuery.select(sql`1`.as("group_marker"));
  }

  if (groupByColumns.length > 0) {
    groupQuery = groupQuery.groupBy(groupByColumns as any);
  }

  if (having && having.length > 0) {
    const havingExpressions: Expression<SqlBool>[] = [];

    for (const condition of having) {
      if (condition.type === "groupKey") {
        const expression = groupKeyExpressions.get(condition.key);
        assert(
          expression,
          `HAVING groupKey ${condition.key} must reference a defined groupBy alias`
        );
        havingExpressions.push(
          applyHavingComparison(
            expression.order,
            condition.operator,
            condition.value
          )
        );
      } else {
        const aggregateExpr = buildAggregateExpression(
          condition.function,
          condition.column,
          schema
        );
        havingExpressions.push(
          applyHavingComparison(
            aggregateExpr,
            condition.operator,
            condition.value
          )
        );
      }
    }

    if (havingExpressions.length === 1) {
      groupQuery = groupQuery.having(havingExpressions[0]);
    } else if (havingExpressions.length > 1) {
      let combined = havingExpressions[0];
      for (let i = 1; i < havingExpressions.length; i++) {
        const prev = combined;
        const next = havingExpressions[i];
        combined = sql`(${prev}) AND (${next})`;
      }
      groupQuery = groupQuery.having(combined);
    }
  }

  const groupedAlias = "grouped_results";
  const groupedQuery = groupQuery.as(groupedAlias);
  let outerQuery = dbForQuery.selectFrom(groupedQuery);

  const reducers: Partial<Record<keyof typeof baseAggregations, Reducer[]>> = {
    count: operationParameters.reducers?.count ?? ["sum"],
    countDistinct: operationParameters.reducers?.countDistinct ?? ["sum"],
    sum: operationParameters.reducers?.sum ?? ["sum"],
    min: operationParameters.reducers?.min ?? [],
    max: operationParameters.reducers?.max ?? [],
    avg: operationParameters.reducers?.avg ?? [],
  };

  const applyReducer = (
    columnAlias: string,
    reducer: Reducer,
    family: keyof typeof baseAggregations
  ) => {
    const ref = sql.ref(columnAlias);
    switch (reducer) {
      case "sum":
        if (env.DATABASE_DIALECT === "mssql" && (family === "count" || family === "countDistinct")) {
          return sql`SUM(CAST(${ref} AS BIGINT))`;
        }
        return sql`SUM(${ref})`;
      case "min":
        return sql`MIN(${ref})`;
      case "max":
        return sql`MAX(${ref})`;
      case "avg":
        return sql`AVG(${ref})`;
    }
  };

  const selectFields: any[] = [];

  if (operationParameters.aggregates.groupCount) {
    if (env.DATABASE_DIALECT === "mssql") {
      selectFields.push(sql`COUNT_BIG(*)`.as("groupCount"));
    } else {
      selectFields.push(sql`COUNT(*)`.as("groupCount"));
    }
  }

  const buildReducerObjects = (
    family: keyof typeof baseAggregations,
    familyFields: [string, any][] | undefined
  ) => {
    if (!familyFields) return undefined;
    const familyReducers = reducers[family];
    if (!familyReducers || familyReducers.length === 0) return undefined;

    const columnObjects: [string, any][] = [];
    for (const [column] of familyFields) {
      const perGroupAlias = perGroupAliases.get(`${family}|${column}`);
      if (!perGroupAlias) continue;
      const reducerFields: [string, any][] = familyReducers.map((reducer) => [
        reducer,
        applyReducer(perGroupAlias, reducer, family as keyof typeof baseAggregations),
      ]);
      const jsonKey = getJsonKeyForReducer(column);
      columnObjects.push([jsonKey, buildJsonObject(reducerFields)]);
    }
    if (columnObjects.length === 0) return undefined;
    return buildJsonObject(columnObjects);
  };

  const countReduced = buildReducerObjects("count", baseAggregations.count);
  if (countReduced) {
    selectFields.push(countReduced.as("_count"));
  }
  const countDistinctReduced = buildReducerObjects(
    "countDistinct",
    baseAggregations.countDistinct
  );
  if (countDistinctReduced) {
    selectFields.push(countDistinctReduced.as("_countDistinct"));
  }
  const sumReduced = buildReducerObjects("sum", baseAggregations.sum);
  if (sumReduced) {
    selectFields.push(sumReduced.as("_sum"));
  }
  const minReduced = buildReducerObjects("min", baseAggregations.min);
  if (minReduced) {
    selectFields.push(minReduced.as("_min"));
  }
  const maxReduced = buildReducerObjects("max", baseAggregations.max);
  if (maxReduced) {
    selectFields.push(maxReduced.as("_max"));
  }
  const avgReduced = buildReducerObjects("avg", baseAggregations.avg);
  if (avgReduced) {
    selectFields.push(avgReduced.as("_avg"));
  }

  if (selectFields.length === 0) {
    // If nothing was requested, at least return group count.
    selectFields.push(sql`COUNT(*)`.as("groupCount"));
  }

  outerQuery = outerQuery.select(selectFields);

  const compiled = outerQuery.compile();
  const [result] = await outerQuery.execute();

  const parsed = result
    ? (parseJsonStrings(result) as Record<string, unknown>)
    : undefined;

  if (parsed) {
    // Keep reducer objects nested (column -> reducer -> value) for clarity.
    const parseJsonField = (value: unknown) => {
      if (value == null) return value;
      if (Buffer.isBuffer(value)) {
        const asString = value.toString("utf8");
        try {
          return JSON.parse(asString);
        } catch {
          return asString;
        }
      }
      if (typeof value === "string") {
        const trimmed = value.trim();
        if (
          (trimmed.startsWith("{") && trimmed.endsWith("}")) ||
          (trimmed.startsWith("[") && trimmed.endsWith("]"))
        ) {
          try {
            return JSON.parse(trimmed);
          } catch {
            return value;
          }
        }
      }
      return value;
    };

    for (const key of ["_avg", "_sum", "_min", "_max", "_count", "_countDistinct"]) {
      const value = parsed[key];
      parsed[key] = parseJsonField(value);
    }

    if (typeof parsed.groupCount === "string" && /^\d+$/.test(parsed.groupCount)) {
      parsed.groupCount = Number(parsed.groupCount);
    }
    aliasRenameMap.forEach((original, sanitized) => {
      Object.keys(parsed).forEach((key) => {
        if (typeof parsed[key] !== "object" || !parsed[key]) return;
        const obj = parsed[key] as Record<string, unknown>;
        if (sanitized in obj && original !== sanitized) {
          obj[original] = obj[sanitized];
          delete obj[sanitized];
        }
      });
    });
  }

  return {
    query: { sql: compiled.sql, params: compiled.parameters },
    data: parsed,
  };
}
