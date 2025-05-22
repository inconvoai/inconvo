import { type Query } from "~/types/querySchema";
import { asc, avg, count, desc, eq, max, min, SQL, sum } from "drizzle-orm";
import { parsePrismaWhere } from "~/operations/utils/prismaToDrizzleWhereConditions";
import { findRelationsBetweenTables } from "~/operations/utils/findRelationsBetweenTables";
import { loadDrizzleSchema } from "~/util/loadDrizzleSchema";
import { buildJsonObjectSelect } from "~/operations/utils/jsonBuilderHelpers";
import { getColumnFromTable } from "~/operations/utils/getColumnFromTable";
import { createAggregationFields } from "~/operations/utils/createAggregationFields";
import assert from "assert";

export async function groupBy(db: any, query: Query) {
  assert(query.operation === "groupBy", "Invalid inconvo operation");
  const { table, whereAndArray, operationParameters, computedColumns } = query;

  const drizzleSchema = await loadDrizzleSchema();

  const countJsonFields = createAggregationFields(
    operationParameters.count?.columns,
    count,
    drizzleSchema,
    computedColumns
  );

  const minJsonFields = createAggregationFields(
    operationParameters.min?.columns,
    min,
    drizzleSchema,
    computedColumns
  );

  const maxJsonFields = createAggregationFields(
    operationParameters.max?.columns,
    max,
    drizzleSchema,
    computedColumns
  );

  const sumJsonFields = createAggregationFields(
    operationParameters.sum?.columns,
    sum,
    drizzleSchema,
    computedColumns
  );

  const avgJsonFields = createAggregationFields(
    operationParameters.avg?.columns,
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

  const groupBySelectFields: Record<string, any> =
    operationParameters.groupBy.reduce((acc, columnIdentifier) => {
      assert(
        columnIdentifier.split(".").length === 2,
        "Invalid column format for group by (not table.column)"
      );
      const [tableName, columnName] = columnIdentifier.split(".");
      const column = getColumnFromTable({
        columnName,
        tableName,
        drizzleSchema,
        computedColumns,
      });
      acc[`${tableName}.${columnName}`] = column;
      return acc;
    }, {} as Record<string, any>);

  const groupByColumns = operationParameters.groupBy.map((columnIdentifier) => {
    assert(
      columnIdentifier.split(".").length === 2,
      "Invalid column format for group by (not table.column)"
    );
    const [tableName, columnName] = columnIdentifier.split(".");
    return getColumnFromTable({
      columnName,
      tableName,
      drizzleSchema,
      computedColumns,
    });
  });

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
      assert(
        operationParameters.orderBy.column.split(".").length === 2,
        "Order By column must be in the format table.column"
      );
      const [tableName, columnName] =
        operationParameters.orderBy.column.split(".");
      const column = getColumnFromTable({
        columnName,
        tableName,
        drizzleSchema,
        computedColumns,
      });
      const direction =
        operationParameters.orderBy.direction === "asc" ? asc : desc;

      switch (operationParameters.orderBy.function) {
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
