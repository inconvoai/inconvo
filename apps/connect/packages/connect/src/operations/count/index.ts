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

export async function count(db: any, query: Query) {
  assert(query.operation === "count", "Invalid inconvo operation");
  const { table, whereAndArray, operationParameters, computedColumns } = query;

  const drizzleSchema = await loadDrizzleSchema();

  const countColumns: [string, SQL<unknown>][] = operationParameters.count.map(
    (columnName) => {
      if (columnName === "_all") {
        return [columnName, dCount()];
      }
      return [
        columnName,
        dCount(
          getColumnFromTable({
            columnName,
            tableName: table,
            drizzleSchema,
            computedColumns,
          })
        ),
      ];
    }
  );

  const distinctColumns: [string, SQL<unknown>][] | null =
    operationParameters.countDistinct?.map((columnName) => {
      return [
        columnName,
        dDistinctCount(
          getColumnFromTable({
            columnName,
            tableName: table,
            drizzleSchema,
            computedColumns,
          })
        ),
      ];
    }) ?? null;

  const dbQuery = db
    .select({
      ["_count"]: buildJsonObjectSelect(countColumns),
      ...(distinctColumns
        ? { ["_countDistinct"]: buildJsonObjectSelect(distinctColumns) }
        : {}),
    })
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

  const response = await dbQuery;

  return { query: dbQuery.toSQL(), data: response[0] };
}
