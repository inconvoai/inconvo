import { type Query } from "~/types/querySchema";
import { asc, avg, count, desc, eq, max, min, sum } from "drizzle-orm";
import { parsePrismaWhere } from "~/operations/utils/prismaToDrizzleWhereConditions";
import { findRelationsBetweenTables } from "~/operations/utils/findRelationsBetweenTables";
import { loadDrizzleSchema } from "~/util/loadDrizzleSchema";
import { buildJsonObjectSelect } from "~/operations/utils/jsonBuilderHelpers";
import { getColumnFromTable } from "~/operations/utils/getColumnFromTable";
import { createAggregationFields } from "~/operations/utils/createAggregationFields";
import assert from "assert";
import { buildDateIntervalExpression } from "~/operations/utils/buildDateIntervalExpression";
import { buildDateComponentExpressions } from "~/operations/utils/buildDateComponentExpression";

export async function groupBy(db: any, query: Query) {
  assert(query.operation === "groupBy", "Invalid inconvo operation");
  const { table, whereAndArray, operationParameters, computedColumns } = query;

  const drizzleSchema = await loadDrizzleSchema();

  const countJsonFields = createAggregationFields(
    operationParameters.count ?? undefined,
    count,
    drizzleSchema,
    computedColumns
  );

  const minJsonFields = createAggregationFields(
    operationParameters.min ?? undefined,
    min,
    drizzleSchema,
    computedColumns
  );

  const maxJsonFields = createAggregationFields(
    operationParameters.max ?? undefined,
    max,
    drizzleSchema,
    computedColumns
  );

  const sumJsonFields = createAggregationFields(
    operationParameters.sum ?? undefined,
    sum,
    drizzleSchema,
    computedColumns
  );

  const avgJsonFields = createAggregationFields(
    operationParameters.avg ?? undefined,
    avg,
    drizzleSchema,
    computedColumns
  );

  const selectFields: Record<string, any> = {};

  if (countJsonFields) {
    selectFields["_count"] = buildJsonObjectSelect(countJsonFields);
  }
  if (minJsonFields) {
    selectFields["_min"] = buildJsonObjectSelect(minJsonFields);
  }
  if (maxJsonFields) {
    selectFields["_max"] = buildJsonObjectSelect(maxJsonFields);
  }
  if (sumJsonFields) {
    selectFields["_sum"] = buildJsonObjectSelect(sumJsonFields);
  }
  if (avgJsonFields) {
    selectFields["_avg"] = buildJsonObjectSelect(avgJsonFields);
  }

  const groupBySelectFields: Record<string, any> = {};
  const groupByColumns: any[] = [];
  const groupKeyExpressions = new Map<string, { select: any; order: any }>();

  for (const key of operationParameters.groupBy) {
    if (key.type === "column") {
      assert(
        key.column.split(".").length === 2,
        "Invalid column format for group by (not table.column)"
      );
      const [tableName, columnName] = key.column.split(".");
      const column = getColumnFromTable({
        columnName,
        tableName,
        drizzleSchema,
        computedColumns,
      });
      const alias = key.alias ?? `${tableName}.${columnName}`;
      groupBySelectFields[alias] = column;
      groupByColumns.push(column);
      groupKeyExpressions.set(alias, { select: column, order: column });
    } else if (key.type === "dateInterval") {
      assert(
        key.column.split(".").length === 2,
        "Invalid column format for group by interval (not table.column)"
      );
      const [tableName, columnName] = key.column.split(".");
      const column = getColumnFromTable({
        columnName,
        tableName,
        drizzleSchema,
        computedColumns,
      });
      const alias = key.alias ?? `${tableName}.${columnName}|${key.interval}`;
      const intervalExpression = buildDateIntervalExpression(
        column,
        key.interval
      );
      groupBySelectFields[alias] = intervalExpression;
      groupByColumns.push(intervalExpression);
      groupKeyExpressions.set(alias, {
        select: intervalExpression,
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
        drizzleSchema,
        computedColumns,
      });
      const alias =
        key.alias ?? `${tableName}.${columnName}|${key.component}`;
      const { select, order } = buildDateComponentExpressions(
        column,
        key.component
      );
      groupBySelectFields[alias] = select;
      groupByColumns.push(select);
      groupByColumns.push(order);
      groupKeyExpressions.set(alias, { select, order });
    }
  }

  const orderByParams = operationParameters.orderBy;

  const dbQuery = db
    .select({
      ...groupBySelectFields,
      ...selectFields,
    })
    .from(drizzleSchema[table])
    .groupBy(...groupByColumns)
    .where((columns: Record<string, unknown>) =>
      parsePrismaWhere({
        drizzleSchema,
        tableName: table,
        where: whereAndArray,
        columns,
        computedColumns: computedColumns,
      })
    )
    .orderBy(() => {
      if (orderByParams.type === "groupKey") {
        const { key, direction } = orderByParams;
        const expression = groupKeyExpressions.get(key);
        assert(
          expression,
          `Order By key ${key} must reference a defined groupBy key`
        );
        const sorter = direction === "asc" ? asc : desc;
        return sorter(expression.order);
      }

      assert(
        orderByParams.column.split(".").length === 2,
        "Order By column must be in the format table.column"
      );
      const [tableName, columnName] = orderByParams.column.split(".");
      const column = getColumnFromTable({
        columnName,
        tableName,
        drizzleSchema,
        computedColumns,
      });
      const direction =
        orderByParams.direction === "asc" ? asc : desc;

      switch (orderByParams.function) {
        case "count":
          return direction(count(column));
        case "sum":
          return direction(sum(column));
        case "min":
          return direction(min(column));
        case "max":
          return direction(max(column));
        case "avg":
          return direction(avg(column));
      }
    })
    .limit(operationParameters.limit);

  if (operationParameters.joins) {
    for (const join of operationParameters.joins) {
      const { table: joinTable, joinPath, joinType } = join;

      const [currentKey, relatedKey] = findRelationsBetweenTables(
        table,
        joinTable,
        joinPath.split(".").at(-1) ?? "",
        drizzleSchema
      );

      switch (joinType) {
        case "inner": {
          dbQuery.innerJoin(
            drizzleSchema[joinTable],
            eq(
              drizzleSchema[table][currentKey],
              drizzleSchema[joinTable][relatedKey]
            )
          );
          break;
        }
        case "left": {
          dbQuery.leftJoin(
            drizzleSchema[joinTable],
            eq(
              drizzleSchema[table][currentKey],
              drizzleSchema[joinTable][relatedKey]
            )
          );
          break;
        }
        case "right": {
          dbQuery.rightJoin(
            drizzleSchema[joinTable],
            eq(
              drizzleSchema[table][currentKey],
              drizzleSchema[joinTable][relatedKey]
            )
          );
          break;
        }
      }
    }
  }

  const response = await dbQuery;

  return { query: dbQuery.toSQL(), data: response };
}
