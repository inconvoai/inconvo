import { type PrismaClient } from "@prisma/client";
import { WhereConditions, type Query } from "~/types/querySchema";
import assert from "assert";
import { splitWhereConditions } from "../utils";
import { dbFindMany } from "./dbFindMany";
import {
  computedFindMany,
  selectOnlyComputedFindMany,
} from "./computedFindMany";
import { generatePrismaClientWithComputedColumns } from "~/util/generatePrismaClientWithComputedColumns";
import { findManyJson } from "./json";

export async function findMany(prisma: PrismaClient, query: Query) {
  assert(query.operation === "findMany", "Invalid inconvo operation");
  const {
    table,
    whereAndArray,
    operationParameters,
    computedColumns,
    jsonColumnSchema,
  } = query;

  const isComputedColumnInOpParams = (
    selectColumns: Record<string, string[] | null>,
    computedColNames: string[] | undefined,
    computedWhere: WhereConditions,
    orderByColumn: string | undefined
  ): boolean => {
    if (!computedColNames) {
      return false;
    }

    if (orderByColumn && computedColNames.includes(orderByColumn)) {
      return true;
    }

    if (computedWhere.length > 0) {
      return true;
    }

    const selectedComputedCol = Object.values(selectColumns)
      .filter((col) => col !== null)
      .some((colArray) =>
        colArray!.some((col) => computedColNames.includes(col))
      );

    if (selectedComputedCol) {
      return true;
    }

    return false;
  };

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
    const computedColumnOnlyInSelect =
      computedWhere.length === 0 &&
      operationParameters.orderBy?.column &&
      !computedColumnNames?.includes(operationParameters.orderBy.column);
    if (computedColumnOnlyInSelect) {
      return selectOnlyComputedFindMany(xPrisma, query);
    } else {
      return computedFindMany(xPrisma, query);
    }
    // TODO: This could be a better check
    // i.e see if any of the jsonColumnSchema tables and columns are in the query
  } else if (jsonColumnSchema && jsonColumnSchema.length > 0) {
    return findManyJson(prisma, query);
  } else {
    return dbFindMany(prisma, query);
  }
}
