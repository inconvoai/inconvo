import { type Query } from "~/types/querySchema";
import {
  count as drizzleCount,
  countDistinct as drizzleCountDistinct,
  eq,
  SQL,
} from "drizzle-orm";
import { parsePrismaWhere } from "~/operations/utils/prismaToDrizzleWhereConditions";
import { loadDrizzleSchema } from "~/util/loadDrizzleSchema";
import assert from "assert";
import { getColumnFromTable } from "../utils/getColumnFromTable";
import { buildJsonObjectSelect } from "../utils/jsonBuilderHelpers";
import { findRelationsBetweenTables } from "../utils/findRelationsBetweenTables";

function createAggregationFields<T>(
  columns: string[] | undefined,
  aggregationFn: (column: SQL<any>) => SQL<T>,
  drizzleSchema: any,
  computedColumns: any
): [string, SQL<T>][] | undefined {
  return columns?.map((columnIdentifier) => {
    assert(
      columnIdentifier.split(".").length === 2,
      "Invalid column format for aggregation (not table.column)"
    );
    const [tableName, columnName] = columnIdentifier.split(".");
    return [
      `${tableName}.${columnName}`,
      aggregationFn(
        getColumnFromTable({
          columnName,
          tableName,
          drizzleSchema,
          computedColumns,
        })
      ),
    ];
  });
}

export async function countWithJoin(db: any, query: Query) {
  assert(query.operation === "countWithJoin", "Invalid inconvo operation");
  const { table, whereAndArray, operationParameters, computedColumns } = query;

  const drizzleSchema = await loadDrizzleSchema();

  const countColumns = createAggregationFields(
    operationParameters.count,
    drizzleCount,
    drizzleSchema,
    computedColumns
  );

  const countDistinctColumns = operationParameters.countDistinct
    ? createAggregationFields(
        operationParameters.countDistinct,
        drizzleCountDistinct,
        drizzleSchema,
        computedColumns
      )
    : undefined;

  assert(countColumns, "Count columns are required");

  const selectFields: any = {};

  if (countColumns) {
    selectFields["_count"] = buildJsonObjectSelect(countColumns);
  }

  if (countDistinctColumns) {
    selectFields["_countDistinct"] =
      buildJsonObjectSelect(countDistinctColumns);
  }

  const dbQuery = db
    .select(selectFields)
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
