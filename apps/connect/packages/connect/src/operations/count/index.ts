import { type PrismaClient } from "@prisma/client";
import { type Query } from "~/types/querySchema";
import assert from "assert";
import { generatePrismaClientWithComputedColumns } from "~/util/generatePrismaClientWithComputedColumns";
import {
  filterResponseWithComputedConditions,
  splitWhereConditions,
} from "../utils";
import { countWithComputedColumn } from "./computed";
import { json } from "stream/consumers";
import { countJson } from "./json";

export async function count(prisma: PrismaClient, query: Query) {
  assert(query.operation === "count", "Invalid inconvo operation");
  const {
    table,
    operation,
    whereAndArray,
    operationParameters,
    computedColumns,
    jsonColumnSchema,
  } = query;

  const [computedWhere, dbWhere] = splitWhereConditions(
    computedColumns || [],
    whereAndArray
  );

  if (computedWhere.length > 0) {
    return countWithComputedColumn(prisma, query);
  }

  const columnNames = operationParameters.columns;
  const jsonColumnNames = jsonColumnSchema
    ? jsonColumnSchema.jsonSchema.map((column) => column.key)
    : [];

  if (
    jsonColumnSchema &&
    columnNames.some((col) => jsonColumnNames.includes(col))
  ) {
    return countJson(prisma, query);
  }

  const selectObject = operationParameters.columns
    ? operationParameters?.columns.reduce(
        (acc: Record<string, boolean>, column) => {
          acc[column] = true;
          return acc;
        },
        {}
      )
    : undefined;

  assert(
    // @ts-ignore
    typeof prisma[table][operation] === "function",
    "Invalid prisma operation"
  );
  // @ts-ignore
  const prismaQuery: Function = prisma[table][operation];
  const response = await prismaQuery({
    select: selectObject,
    where: {
      AND: [...(whereAndArray || [])],
    },
  });

  return response;
}
