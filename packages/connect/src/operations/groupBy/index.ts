import { Kysely, sql } from "kysely";
import type { Expression, SqlBool } from "kysely";
import type { Query } from "../../types/querySchema";
import { buildWhereConditions } from "../utils/whereConditionBuilder";
import { getColumnFromTable } from "../utils/computedColumns";
import {
  buildJsonObject,
  shouldUseFlatAggregates,
  buildFlatAggregateSelections,
  reconstructNestedFromFlat,
} from "../utils/jsonBuilderHelpers";
import { createAggregationFields } from "../utils/createAggregationFields";
import { applyLimit } from "../utils/queryHelpers";
import { getSchemaBoundDb } from "../utils/schemaHelpers";
import { getTableIdentifier } from "../utils/tableIdentifier";
import { buildDateIntervalExpression } from "../utils/buildDateIntervalExpression";
import { buildDateComponentExpressions } from "../utils/buildDateComponentExpression";
import assert from "assert";
import { applyJoinHop, normaliseJoinHop } from "../utils/joinDescriptorHelpers";
import { parseJsonStrings, flattenObjectKeys } from "../utils/jsonParsing";
import { buildAggregateExpression } from "../utils/aggregateExpressionBuilder";
import { applyHavingComparison } from "../utils/havingComparison";
import { executeWithLogging } from "../utils/executeWithLogging";
import type { OperationContext } from "../types";

export async function groupBy(
  db: Kysely<any>,
  query: Query,
  ctx: OperationContext,
) {
  assert(query.operation === "groupBy", "Invalid operation");
  const { table, whereAndArray, operationParameters } = query;

  const { schema, dialect } = ctx;
  const dbForQuery = getSchemaBoundDb(db, schema, dialect);
  const { groupBy: groupByList, orderBy, limit, joins } = operationParameters;
  const having = operationParameters.having ?? null;

  // Build query with schema-qualified table name
  const tableId = getTableIdentifier(table, query.tableSchema, dialect);
  let dbQuery = dbForQuery.selectFrom(tableId);

  // Handle joins if specified - deduplicate hops to avoid duplicate table joins
  if (joins && joins.length > 0) {
    const appliedHops = new Set<string>();
    for (const join of joins) {
      const joinType = join.joinType ?? "left";
      for (const hop of join.path) {
        // Create a unique key for this hop based on source and target
        const hopKey = `${[...hop.source].sort().join(",")}|${[...hop.target].sort().join(",")}`;
        if (appliedHops.has(hopKey)) {
          continue; // Skip duplicate hop
        }
        appliedHops.add(hopKey);
        const metadata = normaliseJoinHop(hop);
        dbQuery = applyJoinHop(dbQuery, joinType, metadata, schema, dialect);
      }
    }
  }

  // Build aggregation fields using helper function
  const countJsonFields = createAggregationFields(
    operationParameters.count ?? undefined,
    (column) => sql`COUNT(${column})`,
    schema,
    dialect,
  );

  const countDistinctJsonFields = createAggregationFields(
    operationParameters.countDistinct ?? undefined,
    (column) => sql`COUNT(DISTINCT ${column})`,
    schema,
    dialect,
  );

  const minJsonFields = createAggregationFields(
    operationParameters.min ?? undefined,
    (column) => sql`MIN(${column})`,
    schema,
    dialect,
  );

  const maxJsonFields = createAggregationFields(
    operationParameters.max ?? undefined,
    (column) => sql`MAX(${column})`,
    schema,
    dialect,
  );

  const sumJsonFields = createAggregationFields(
    operationParameters.sum ?? undefined,
    (column) => sql`SUM(${column})`,
    schema,
    dialect,
  );

  const avgJsonFields = createAggregationFields(
    operationParameters.avg ?? undefined,
    (column) => sql`AVG(${column})`,
    schema,
    dialect,
  );

  const selectFields: Record<string, any> = {};
  const useFlatAggregates = shouldUseFlatAggregates(dialect);
  const flatAggregateSelections: [string, any][] = [];

  if (useFlatAggregates) {
    // For MSSQL, use flat columns instead of JSON subqueries
    flatAggregateSelections.push(
      ...buildFlatAggregateSelections("_count", countJsonFields),
      ...buildFlatAggregateSelections(
        "_countDistinct",
        countDistinctJsonFields,
      ),
      ...buildFlatAggregateSelections("_min", minJsonFields),
      ...buildFlatAggregateSelections("_max", maxJsonFields),
      ...buildFlatAggregateSelections("_sum", sumJsonFields),
      ...buildFlatAggregateSelections("_avg", avgJsonFields),
    );
  } else {
    // For other dialects, use JSON wrapping
    if (countJsonFields) {
      selectFields["_count"] = buildJsonObject(countJsonFields, dialect);
    }
    if (countDistinctJsonFields) {
      selectFields["_countDistinct"] = buildJsonObject(countDistinctJsonFields, dialect);
    }
    if (minJsonFields) {
      selectFields["_min"] = buildJsonObject(minJsonFields, dialect);
    }
    if (maxJsonFields) {
      selectFields["_max"] = buildJsonObject(maxJsonFields, dialect);
    }
    if (sumJsonFields) {
      selectFields["_sum"] = buildJsonObject(sumJsonFields, dialect);
    }
    if (avgJsonFields) {
      selectFields["_avg"] = buildJsonObject(avgJsonFields, dialect);
    }
  }

  const aliasRenameMap = new Map<string, string>();
  const getDialectAlias = (alias: string) => {
    if (dialect === "bigquery") {
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
  const groupKeyExpressions = new Map<
    string,
    { select: any; order: any; having: any }
  >();

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
        dialect,
      });
      const alias = key.alias ?? `${tableName!}.${columnName!}`;
      const dialectAlias = getDialectAlias(alias);
      selections.push(sql`${column}`.as(dialectAlias));
      groupByColumns.push(column);
      groupKeyExpressions.set(alias, {
        select: column,
        order: column,
        having: column,
      });
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
        dialect,
      });
      const alias = key.alias ?? `${tableName!}.${columnName!}|${key.interval}`;
      const dialectAlias = getDialectAlias(alias);
      const intervalExpression = buildDateIntervalExpression(
        column,
        key.interval,
        dialect,
      );
      selections.push(intervalExpression.as(dialectAlias));
      groupByColumns.push(intervalExpression);
      // MySQL and BigQuery support alias in HAVING; PostgreSQL/MSSQL require the expression
      const havingExpr =
        dialect === "mysql" || dialect === "bigquery"
          ? sql.ref(dialectAlias)
          : intervalExpression;
      groupKeyExpressions.set(alias, {
        select: intervalExpression,
        order: intervalExpression,
        having: havingExpr,
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
        dialect,
      });
      const alias =
        key.alias ?? `${tableName!}.${columnName!}|${key.component}`;
      const dialectAlias = getDialectAlias(alias);
      const { select, order } = buildDateComponentExpressions(
        column,
        key.component,
        dialect,
      );
      selections.push(select.as(dialectAlias));
      groupByColumns.push(select);
      groupByColumns.push(order);
      // BigQuery requires using alias in HAVING clause for computed expressions.
      // We add a hidden numeric column for HAVING comparisons to ensure consistency.
      let havingExpr: any;
      if (dialect === "bigquery") {
        const orderAlias = `${dialectAlias}__order`;
        selections.push(order.as(orderAlias));
        havingExpr = sql.ref(orderAlias);
      } else {
        havingExpr = order;
      }
      groupKeyExpressions.set(alias, { select, order, having: havingExpr });
    }
  }

  // Add aggregation fields to selections
  if (useFlatAggregates) {
    // For MSSQL, add flat aggregate columns
    for (const [alias, expr] of flatAggregateSelections) {
      selections.push(sql`${expr}`.as(alias));
    }
  } else {
    // For other dialects, add JSON-wrapped aggregates
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
  }

  // Apply WHERE conditions before grouping
  const whereExpr = buildWhereConditions(whereAndArray, table, schema, dialect, query.tableConditions);
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
            expression.having,
            condition.operator,
            condition.value,
          ),
        );
      } else {
        const aggregateExpr = buildAggregateExpression(
          condition.function,
          condition.column,
          schema,
          dialect,
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
      dialect,
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
  dbQuery = applyLimit(dbQuery, limit, dialect);

  const { rows: response, compiled } = await executeWithLogging(dbQuery, {
    operation: "groupBy",
  });

  const aggregatePrefixes = [
    "_avg",
    "_sum",
    "_min",
    "_max",
    "_count",
    "_countDistinct",
  ];

  const normalisedRows = response.map((row) => {
    let parsed = parseJsonStrings(row) as Record<string, unknown>;

    if (useFlatAggregates) {
      // For MSSQL, reconstruct nested objects from flat column names
      parsed = reconstructNestedFromFlat(parsed, aggregatePrefixes);
    } else {
      // For other dialects, flatten the JSON object keys
      for (const key of aggregatePrefixes) {
        const value = parsed[key];
        if (value && typeof value === "object" && !Array.isArray(value)) {
          parsed[key] = flattenObjectKeys(value as Record<string, unknown>);
        }
      }
    }

    aliasRenameMap.forEach((original, sanitized) => {
      if (sanitized in parsed && original !== sanitized) {
        parsed[original] = parsed[sanitized];
        delete parsed[sanitized];
      }
    });

    // Remove hidden __order columns used for BigQuery HAVING comparisons
    for (const key of Object.keys(parsed)) {
      if (key.endsWith("__order")) {
        delete parsed[key];
      }
    }

    return parsed;
  });

  return {
    query: { sql: compiled.sql, params: compiled.parameters },
    data: normalisedRows,
  };
}
