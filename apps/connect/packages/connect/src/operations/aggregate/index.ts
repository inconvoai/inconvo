import { type PrismaClient } from "@prisma/client";
import { type Query } from "~/types/querySchema";
import { splitWhereConditions } from "../utils";
import { aggregateWithComputedColumn } from "./computed";
import { aggregateJson } from "./json";
import assert from "assert";

export async function aggregate(prisma: PrismaClient, query: Query) {
  assert(query.operation === "aggregate", "Invalid inconvo operation");
  const {
    table,
    operation,
    whereAndArray,
    operationParameters,
    computedColumns,
    jsonColumnSchema,
  } = query;

  const computedColumnNames = computedColumns?.map((column) => column.name);
  const jsonColumnSchemaEntry = jsonColumnSchema?.find(
    (x) => x.tableName === table
  );
  const jsonColumnNames = jsonColumnSchemaEntry
    ? jsonColumnSchemaEntry.jsonSchema.map((column) => column.name)
    : [];
  const columnNames = operationParameters.columns;

  const [computedWhere, _dbWhere] = splitWhereConditions(
    computedColumns || [],
    whereAndArray
  );

  const computedColumnUsed = !computedColumnNames
    ? false
    : columnNames.some((col) => computedColumnNames.includes(col));

  if (computedColumnUsed || computedWhere.length > 0) {
    return aggregateWithComputedColumn(prisma, query);
  }

  if (
    jsonColumnSchema &&
    columnNames.some((col) => jsonColumnNames.includes(col))
  ) {
    return aggregateJson(prisma, query);
  }

  const columns = operationParameters.columns.reduce(
    (acc: { [key: string]: boolean }, column: string) => {
      acc[column] = true;
      return acc;
    },
    {}
  );
  assert(
    // @ts-ignore
    typeof prisma[table][operation] === "function",
    "Invalid prisma operation"
  );
  // @ts-ignore
  const prismaQuery: Function = prisma[table][operation];
  const response = await prismaQuery({
    _avg: { ...columns },
    _sum: { ...columns },
    _min: { ...columns },
    _max: { ...columns },
    _count: { ...columns },
    where: { AND: [...(whereAndArray || [])] },
  });
  return response;
}
