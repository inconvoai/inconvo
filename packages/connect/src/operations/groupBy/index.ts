import { Kysely, sql } from "kysely";
import type { Expression, SqlBool } from "kysely";
import type { Query } from "../../types/querySchema";
import { buildWhereConditions } from "../utils/whereConditionBuilder";
import { getAugmentedSchema } from "../../util/augmentedSchemaCache";
import { getColumnFromTable } from "../utils/computedColumns";
import { buildJsonObject } from "../utils/jsonBuilderHelpers";
import { createAggregationFields } from "../utils/createAggregationFields";
import { applyLimit } from "../utils/queryHelpers";
import { getSchemaBoundDb } from "../utils/schemaHelpers";
import { buildDateIntervalExpression } from "../utils/buildDateIntervalExpression";
import { buildDateComponentExpressions } from "../utils/buildDateComponentExpression";
import assert from "assert";
import { applyJoinHop, normaliseJoinHop } from "../utils/joinDescriptorHelpers";
import { parseJsonStrings, flattenObjectKeys } from "../utils/jsonParsing";
import { buildAggregateExpression } from "../utils/aggregateExpressionBuilder";
import { applyHavingComparison } from "../utils/havingComparison";
import { executeWithLogging } from "../utils/executeWithLogging";
import { env } from "../../env";

export async function groupBy(db: Kysely<any>, query: Query) {
  assert(query.operation === "groupBy", "Invalid operation");
  const { table, whereAndArray, operationParameters } = query;

  const schema = await getAugmentedSchema();
  const dbForQuery = getSchemaBoundDb(db, schema);
  const { groupBy: groupByList, orderBy, limit, joins } = operationParameters;
  const having = operationParameters.having ?? null;

  let dbQuery = dbForQuery.selectFrom(table);

  // Handle joins if specified
  if (joins && joins.length > 0) {
    for (const join of joins) {
      const joinType = join.joinType ?? "left";
      for (const hop of join.path) {
        const metadata = normaliseJoinHop(hop);
        dbQuery = applyJoinHop(dbQuery, joinType, metadata);
      }
    }
  }

  // Build aggregation fields using helper function
  const countJsonFields = createAggregationFields(
    operationParameters.count ?? undefined,
    (column) => sql`COUNT(${column})`,
    schema,
  );

  const countDistinctJsonFields = createAggregationFields(
    operationParameters.countDistinct ?? undefined,
    (column) => sql`COUNT(DISTINCT ${column})`,
    schema,
  );

  const minJsonFields = createAggregationFields(
    operationParameters.min ?? undefined,
    (column) => sql`MIN(${column})`,
    schema,
  );

  const maxJsonFields = createAggregationFields(
    operationParameters.max ?? undefined,
    (column) => sql`MAX(${column})`,
    schema,
  );

  const sumJsonFields = createAggregationFields(
    operationParameters.sum ?? undefined,
    (column) => sql`SUM(${column})`,
    schema,
  );

  const avgJsonFields = createAggregationFields(
    operationParameters.avg ?? undefined,
    (column) => sql`AVG(${column})`,
    schema,
  );

  const selectFields: Record<string, any> = {};

  if (countJsonFields) {
    selectFields["_count"] = buildJsonObject(countJsonFields);
  }
  if (countDistinctJsonFields) {
    selectFields["_countDistinct"] = buildJsonObject(countDistinctJsonFields);
  }
  if (minJsonFields) {
    selectFields["_min"] = buildJsonObject(minJsonFields);
  }
  if (maxJsonFields) {
    selectFields["_max"] = buildJsonObject(maxJsonFields);
  }
  if (sumJsonFields) {
    selectFields["_sum"] = buildJsonObject(sumJsonFields);
  }
  if (avgJsonFields) {
    selectFields["_avg"] = buildJsonObject(avgJsonFields);
  }

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

  const selections: any[] = [];

  // Add group by columns to selection
  const groupByColumns: any[] = [];
  const groupKeyExpressions = new Map<string, { select: any; order: any }>();

  for (const key of groupByList) {
    if (key.type === "column") {
      assert(
        key.column.split(".").length === 2,
        "Invalid column format for group by (not table.column)",
      );
      const [tableName, columnName] = key.column.split(".");
      const column = getColumnFromTable({
        columnName: columnName!,
        tableName: tableName!,
        schema,
      });
      const alias = key.alias ?? `${tableName!}.${columnName!}`;
      const dialectAlias = getDialectAlias(alias);
      selections.push(sql`${column}`.as(dialectAlias));
      groupByColumns.push(column);
      groupKeyExpressions.set(alias, { select: column, order: column });
    } else if (key.type === "dateInterval") {
      assert(
        key.column.split(".").length === 2,
        "Invalid column format for group by interval (not table.column)",
      );
      const [tableName, columnName] = key.column.split(".");
      const column = getColumnFromTable({
        columnName: columnName!,
        tableName: tableName!,
        schema,
      });
      const alias = key.alias ?? `${tableName!}.${columnName!}|${key.interval}`;
      const dialectAlias = getDialectAlias(alias);
      const intervalExpression = buildDateIntervalExpression(
        column,
        key.interval,
      );
      selections.push(intervalExpression.as(dialectAlias));
      groupByColumns.push(intervalExpression);
      groupKeyExpressions.set(alias, {
        select: intervalExpression,
        order: intervalExpression,
      });
    } else if (key.type === "dateComponent") {
      assert(
        key.column.split(".").length === 2,
        "Invalid column format for group by component (not table.column)",
      );
      const [tableName, columnName] = key.column.split(".");
      const column = getColumnFromTable({
        columnName: columnName!,
        tableName: tableName!,
        schema,
      });
      const alias = key.alias ?? `${tableName!}.${columnName!}|${key.component}`;
      const dialectAlias = getDialectAlias(alias);
      const { select, order } = buildDateComponentExpressions(
        column,
        key.component,
      );
      selections.push(select.as(dialectAlias));
      groupByColumns.push(select);
      groupByColumns.push(order);
      groupKeyExpressions.set(alias, { select, order });
    }
  }

  // Add aggregation fields to selections
  if (selectFields._count) {
    selections.push(selectFields._count.as("_count"));
  }
  if (selectFields._countDistinct) {
    selections.push(selectFields._countDistinct.as("_countDistinct"));
  }
  if (selectFields._min) {
    selections.push(selectFields._min.as("_min"));
  }
  if (selectFields._max) {
    selections.push(selectFields._max.as("_max"));
  }
  if (selectFields._sum) {
    selections.push(selectFields._sum.as("_sum"));
  }
  if (selectFields._avg) {
    selections.push(selectFields._avg.as("_avg"));
  }

  // Apply WHERE conditions before grouping
  const whereExpr = buildWhereConditions(whereAndArray, table, schema);
  if (whereExpr) {
    dbQuery = dbQuery.where(whereExpr);
  }

  // Apply select list
  if (selections.length > 0) {
    dbQuery = dbQuery.select(selections);
  }

  // Apply group by
  if (groupByColumns.length > 0) {
    dbQuery = dbQuery.groupBy(groupByColumns as any);
  }

  // Apply HAVING (flat AND)
  if (having && having.length > 0) {
    const havingExpressions: Expression<SqlBool>[] = [];

    for (const condition of having) {
      if (condition.type === "groupKey") {
        const expression = groupKeyExpressions.get(condition.key);
        assert(
          expression,
          `HAVING groupKey ${condition.key} must reference a defined groupBy alias`,
        );
        havingExpressions.push(
          applyHavingComparison(
            expression.order,
            condition.operator,
            condition.value,
          ),
        );
      } else {
        const aggregateExpr = buildAggregateExpression(
          condition.function,
          condition.column,
          schema,
        );
        havingExpressions.push(
          applyHavingComparison(
            aggregateExpr,
            condition.operator,
            condition.value,
          ),
        );
      }
    }

    if (havingExpressions.length === 1) {
      dbQuery = dbQuery.having(havingExpressions[0]!);
    } else if (havingExpressions.length > 1) {
      let combined = havingExpressions[0]!;
      for (let i = 1; i < havingExpressions.length; i++) {
        const prev = combined;
        const next = havingExpressions[i]!;
        combined = sql`(${prev}) AND (${next})`;
      }
      dbQuery = dbQuery.having(combined);
    }
  }

  // Apply ordering
  if (orderBy.type === "groupKey") {
    const dir = orderBy.direction === "desc" ? "desc" : "asc";
    const expression = groupKeyExpressions.get(orderBy.key);
    assert(
      expression,
      `Order By key ${orderBy.key} must reference a defined groupBy key`,
    );
    dbQuery = dbQuery.orderBy(expression.order, dir);
  } else {
    assert(
      orderBy.column.split(".").length === 2,
      "Order By column must be in the format table.column",
    );
    const [tableName, columnName] = orderBy.column.split(".");
    const column = getColumnFromTable({
      columnName: columnName!,
      tableName: tableName!,
      schema,
    });
    const dir = orderBy.direction === "desc" ? "desc" : "asc";

    switch (orderBy.function) {
      case "count":
        dbQuery = dbQuery.orderBy(sql`COUNT(${column})`, dir);
        break;
      case "countDistinct":
        dbQuery = dbQuery.orderBy(sql`COUNT(DISTINCT ${column})`, dir);
        break;
      case "sum":
        dbQuery = dbQuery.orderBy(sql`SUM(${column})`, dir);
        break;
      case "min":
        dbQuery = dbQuery.orderBy(sql`MIN(${column})`, dir);
        break;
      case "max":
        dbQuery = dbQuery.orderBy(sql`MAX(${column})`, dir);
        break;
      case "avg":
        dbQuery = dbQuery.orderBy(sql`AVG(${column})`, dir);
        break;
    }
  }

  // Apply limit
  dbQuery = applyLimit(dbQuery, limit);

  const { rows: response, compiled } = await executeWithLogging(dbQuery, {
    operation: "groupBy",
  });
  const normalisedRows = response.map((row) => {
    const parsed = parseJsonStrings(row) as Record<string, unknown>;
    for (const key of [
      "_avg",
      "_sum",
      "_min",
      "_max",
      "_count",
      "_countDistinct",
    ]) {
      const value = parsed[key];
      if (value && typeof value === "object" && !Array.isArray(value)) {
        parsed[key] = flattenObjectKeys(value as Record<string, unknown>);
      }
    }
    aliasRenameMap.forEach((original, sanitized) => {
      if (sanitized in parsed && original !== sanitized) {
        parsed[original] = parsed[sanitized];
        delete parsed[sanitized];
      }
    });
    return parsed;
  });

  return {
    query: { sql: compiled.sql, params: compiled.parameters },
    data: normalisedRows,
  };
}
