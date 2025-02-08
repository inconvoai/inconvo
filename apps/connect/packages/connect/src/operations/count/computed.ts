import { type PrismaClient } from "@prisma/client";
import { type Query } from "~/types/querySchema";
import assert from "assert";
import { generatePrismaClientWithComputedColumns } from "~/util/generatePrismaClientWithComputedColumns";
import {
  filterResponseWithComputedConditions,
  splitWhereConditions,
} from "../utils";

export async function countWithComputedColumn(
  prisma: PrismaClient,
  query: Query
) {
  assert(query.operation === "count", "Invalid inconvo operation");
  const { table, whereAndArray, operationParameters, computedColumns } = query;

  const [computedWhere, dbWhere] = splitWhereConditions(
    computedColumns || [],
    whereAndArray
  );

  const whereObject = {
    AND: [...(dbWhere || [])],
  };

  const xPrisma = generatePrismaClientWithComputedColumns(
    prisma,
    table,
    computedColumns
  );

  const selectColumns = [
    ...operationParameters.columns,
    ...(computedColumns ? computedColumns.map((column) => column.name) : []),
  ].reduce((acc: Record<string, boolean>, column) => {
    acc[column] = true;
    return acc;
  }, {});

  // @ts-expect-error - We don't know the table name in advance
  const prismaQuery = xPrisma[table]["findMany"];
  const response = await prismaQuery({
    select: selectColumns,
    where: whereObject,
  });

  const resWithWhereComputedCondition = filterResponseWithComputedConditions(
    response,
    computedWhere
  );

  const computedWhereColumnNames = computedWhere
    .map((condition) => Object.keys(condition))
    .flat();

  const modifiedData = resWithWhereComputedCondition.map(
    (item: { [x: string]: any }) => {
      return Object.keys(item).reduce((acc, key) => {
        if (!computedWhereColumnNames.includes(key)) {
          acc[key] = item[key];
        }
        return acc;
      }, {} as { [key: string]: any });
    }
  );

  const count = modifiedData.reduce(
    (acc: { [x: string]: number }, item: { [x: string]: null }) => {
      Object.keys(item).forEach((key) => {
        if (item[key] !== null) {
          if (!acc[key]) {
            acc[key] = 0;
          }
          acc[key]++;
        }
      });
      return acc;
    },
    {} as { [key: string]: number }
  );

  return count;
}
