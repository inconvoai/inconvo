import { type PrismaClient } from "@prisma/client";
import { type Query } from "~/types/querySchema";
import assert from "assert";

export async function findDistinct(prisma: PrismaClient, query: Query) {
  assert(query.operation === "findDistinct", "Invalid inconvo operation");
  const { table, whereAndArray, operationParameters } = query;
  const whereObject = {
    AND: [...(whereAndArray || [])],
  };
  // @ts-ignore
  const prismaQuery: Function = prisma[table]["findMany"];
  const response = await prismaQuery({
    select: {
      [operationParameters.column]: true,
    },
    distinct: [operationParameters.column],
    where: whereObject,
  });
  return response;
}
