import { type PrismaClient } from "@prisma/client";
import { type Query } from "~/types/querySchema";
import assert from "assert";
import { generatePrismaClientWithComputedColumns } from "~/util/generatePrismaClientWithComputedColumns";
import {
  filterResponseWithComputedConditions,
  splitWhereConditions,
} from "./utils";

export async function count(prisma: PrismaClient, query: Query) {
  assert(query.operation === "count", "Invalid inconvo operation");
  const {
    table,
    operation,
    whereAndArray,
    operationParameters,
    computedColumns,
  } = query;

  const [computedWhere, dbWhere] = splitWhereConditions(
    computedColumns || [],
    whereAndArray
  );

  const selectObject = operationParameters.columns
    ? operationParameters?.columns.reduce(
        (acc: Record<string, boolean>, column) => {
          acc[column] = true;
          return acc;
        },
        {}
      )
    : undefined;

  const whereObject = {
    AND: [...(dbWhere || [])],
  };

  if (computedWhere.length > 0) {
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

    // @ts-ignore
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

  assert(
    // @ts-ignore
    typeof prisma[table][operation] === "function",
    "Invalid prisma operation"
  );
  // @ts-ignore
  const prismaQuery: Function = prisma[table][operation];
  const response = await prismaQuery({
    select: selectObject,
    where: whereObject,
  });

  return response;
}
