import { type PrismaClient } from "@prisma/client";
import { type Query } from "~/types/querySchema";
import assert from "assert";
import {
  filterResponseWithComputedConditions,
  getSelectColumns,
  splitWhereConditions,
} from "~/operations/utils";
import { generatePrismaClientWithComputedColumns } from "~/util/generatePrismaClientWithComputedColumns";

export async function countRelationsComputed(
  prisma: PrismaClient,
  query: Query
) {
  assert(query.operation === "countRelations", "Invalid inconvo operation");
  const { table, whereAndArray, operationParameters, computedColumns } = query;

  const [computedWhere, dbWhere] = splitWhereConditions(
    computedColumns || [],
    whereAndArray
  );

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

  const prismaClient = generatePrismaClientWithComputedColumns(
    prisma,
    table,
    computedColumns
  );

  // @ts-expect-error   - We don't know the table name in advance
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
  });

  return filterResponseWithComputedConditions(response, computedWhere).slice(
    0,
    operationParameters.limit
  );
}
