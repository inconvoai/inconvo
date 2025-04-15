import { type Query } from "~/types/querySchema";
import { parsePrismaWhere } from "~/operations/utils/prismaToDrizzleWhereConditions";
import { loadDrizzleSchema } from "~/util/loadDrizzleSchema";
import { getColumnFromTable } from "../utils/getColumnFromTable";
import assert from "assert";

export async function findDistinct(db: any, query: Query) {
  assert(query.operation === "findDistinct", "Invalid inconvo operation");
  const { table, whereAndArray, operationParameters, computedColumns } = query;

  const drizzleSchema = await loadDrizzleSchema();

  const distinctColumn = getColumnFromTable({
    columnName: operationParameters.column,
    tableName: table,
    drizzleSchema,
    computedColumns,
  });

  const dbQuery = db

    .selectDistinct({
      [operationParameters.column]: distinctColumn,
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
    )
    .limit(500);

  const response = await dbQuery;
  if (response.length > 499) {
    throw new Error("Find Distinct limit hit at 500");
  }

  return { query: db.toSQL(), data: response };
}
