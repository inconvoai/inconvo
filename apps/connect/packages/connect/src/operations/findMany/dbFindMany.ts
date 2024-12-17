import { PrismaClient } from "@prisma/client";
import assert from "assert";
import { type Query } from "~/types/querySchema";
import { buildRelationalSelect } from "~/util/buildRelationalSelect";

export async function dbFindMany(prisma: PrismaClient, query: Query) {
  assert(query.operation === "findMany", "Invalid inconvo operation");
  const { table, operation, whereAndArray, operationParameters } = query;

  assert(
    // @ts-ignore
    typeof prisma[table][operation] === "function",
    "Invalid prisma operation"
  );
  // @ts-ignore
  const prismaQuery: Function = prisma[table][operation];
  const response = await prismaQuery({
    select: buildRelationalSelect(table, operationParameters.columns),
    where: { AND: [...(whereAndArray || [])] },
    orderBy: operationParameters.orderBy
      ? {
          [operationParameters.orderBy.column]:
            operationParameters.orderBy.direction,
        }
      : undefined,
    take: operationParameters.limit,
  });
  return response;
}
