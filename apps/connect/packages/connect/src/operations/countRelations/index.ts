import { type PrismaClient } from "@prisma/client";
import { type Query } from "~/types/querySchema";
import assert from "assert";
import { getSelectColumns, splitWhereConditions } from "~/operations/utils";
import { generatePrismaClientWithComputedColumns } from "~/util/generatePrismaClientWithComputedColumns";
import { countRelationsComputed } from "./computed";
import { countRelationsJson } from "./json";

export async function countRelations(prisma: PrismaClient, query: Query) {
  assert(query.operation === "countRelations", "Invalid inconvo operation");
  const {
    table,
    whereAndArray,
    operationParameters,
    computedColumns,
    jsonColumnSchema,
  } = query;

  if (jsonColumnSchema && jsonColumnSchema?.length > 0) {
    return countRelationsJson(prisma, query);
  }

  const [computedWhere, dbWhere] = splitWhereConditions(
    computedColumns || [],
    whereAndArray
  );

  if (computedWhere.length > 0) {
    return countRelationsComputed(prisma, query);
  }

  const whereObject = {
    AND: [...(whereAndArray || [])],
  };

  const selectColumns = getSelectColumns(
    operationParameters.columns,
    computedColumns
  );

  const countObject = operationParameters.relations
    ? operationParameters?.relations.reduce(
        (acc: Record<string, boolean>, column) => {
          acc[column] = true;
          return acc;
        },
        {}
      )
    : undefined;

  const prismaClient =
    computedWhere.length > 0
      ? generatePrismaClientWithComputedColumns(prisma, table, computedColumns)
      : prisma;

  // @ts-ignore
  const prismaQuery: Function = prismaClient[table]["findMany"];
  const response = await prismaQuery({
    select: {
      ...selectColumns,
      _count: {
        select: countObject,
      },
    },
    where: whereObject,
    orderBy: operationParameters.orderBy
      ? {
          [operationParameters.orderBy.relation]: {
            _count: operationParameters.orderBy.direction,
          },
        }
      : undefined,
    take: operationParameters.limit,
  });

  return response.length > 0 ? response : 0;
}
