import { type PrismaClient } from "@prisma/client";
import { type Query } from "~/types/querySchema";
import assert from "assert";
import { splitWhereConditions } from "../utils";
import { dbFindMany } from "./dbFindMany";
import {
  computedFindMany,
  selectOnlyComputedFindMany,
} from "./computedFindMany";
import { generatePrismaClientWithComputedColumns } from "~/util/generatePrismaClientWithComputedColumns";

export async function findMany(prisma: PrismaClient, query: Query) {
  assert(query.operation === "findMany", "Invalid inconvo operation");
  const { table, whereAndArray, operationParameters, computedColumns } = query;

  const [computedWhere] = splitWhereConditions(
    computedColumns || [],
    whereAndArray
  );

  const computedColumnNames = computedColumns?.map((column) => column.name);

  const computedColumnInOpParams = isComputedColumnInOpParams(
    operationParameters.columns,
    computedColumnNames,
    computedWhere,
    operationParameters.orderBy?.column
  );

  if (computedColumnInOpParams) {
    const xPrisma = generatePrismaClientWithComputedColumns(
      prisma,
      table,
      computedColumns
    );

    const computedColumnOnlyInSelect = isComputedColumnOnlyInSelect(
      computedWhere,
      computedColumnNames,
      operationParameters.orderBy?.column
    );
    if (computedColumnOnlyInSelect) {
      return selectOnlyComputedFindMany(xPrisma, query);
    } else {
      return computedFindMany(xPrisma, query);
    }
  } else {
    return dbFindMany(prisma, query);
  }
}

const isComputedColumnInOpParams = function (
  selectColumns: any,
  computedColNames: string[] | undefined,
  computedWhere: any,
  orderByColumn: any
) {
  if (!computedColNames) {
    return false;
  }

  const selectedComputedCol = (Object.values(selectColumns) as string[][]).some(
    (colArray: string[]) =>
      colArray.some((col: string) => computedColNames?.includes(col))
  );

  const whereComputedCol = computedWhere.length > 0;
  const orderByComputedCol =
    orderByColumn && computedColNames.includes(orderByColumn);

  return selectedComputedCol || whereComputedCol || orderByComputedCol;
};

const isComputedColumnOnlyInSelect = function (
  computedWhere: any,
  computedColNames: string[] | undefined,
  orderByColumn: any
) {
  return (
    computedWhere.length === 0 && !computedColNames?.includes(orderByColumn)
  );
};
