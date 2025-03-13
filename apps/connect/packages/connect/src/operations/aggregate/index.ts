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

  const columnNames = [
    ...(operationParameters.avg ?? []),
    ...(operationParameters.sum ?? []),
    ...(operationParameters.min ?? []),
    ...(operationParameters.max ?? []),
    ...(operationParameters.count ?? []),
    ...(operationParameters.median ?? []),
  ];

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

  const createColumnObject = (columns: string[] | null) => {
    if (!columns) return {};
    return columns.reduce((acc: { [key: string]: boolean }, column: string) => {
      acc[column] = true;
      return acc;
    }, {});
  };

  const aggregateObject = {
    ...(operationParameters.avg
      ? { _avg: createColumnObject(operationParameters.avg) }
      : {}),
    ...(operationParameters.sum
      ? { _sum: createColumnObject(operationParameters.sum) }
      : {}),
    ...(operationParameters.min
      ? { _min: createColumnObject(operationParameters.min) }
      : {}),
    ...(operationParameters.max
      ? { _max: createColumnObject(operationParameters.max) }
      : {}),
    ...(operationParameters.count
      ? { _count: createColumnObject(operationParameters.count) }
      : {}),
  };

  // @ts-expect-error - We don't know the table name in advance
  const prismaQuery: Function = prisma[table][operation];
  const response = await prismaQuery({
    ...aggregateObject,
    where: { AND: [...(whereAndArray || [])] },
  });

  if (operationParameters.median) {
    // @ts-expect-error - We don't know the table name in advance
    const medianFm = await prisma[table]["findMany"]({
      select: createColumnObject(operationParameters.median),
      where: { AND: [...(whereAndArray || [])] },
    });
    const median = operationParameters.median.reduce((acc, column) => {
      const sorted = medianFm
        .map((x: any) => x[column])
        .sort((a: number, b: number) => a - b);
      const mid = Math.floor(sorted.length / 2);
      return {
        ...acc,
        [column]:
          sorted.length % 2 !== 0
            ? sorted[mid]
            : (sorted[mid - 1] + sorted[mid]) / 2,
      };
    }, {});
    response._median = median;
  }
  return response;
}
