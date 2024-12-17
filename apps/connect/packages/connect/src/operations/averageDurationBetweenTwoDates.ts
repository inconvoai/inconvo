import { type PrismaClient } from "@prisma/client";
import { type Query } from "~/types/querySchema";
import assert from "assert";

export async function averageDurationBetweenTwoDates(
  prisma: PrismaClient,
  query: Query
) {
  assert(
    query.operation === "averageDurationBetweenTwoDates",
    "Invalid inconvo operation"
  );
  const { table, whereAndArray, operationParameters } = query;
  const prismaExtended = prisma.$extends({
    // @ts-ignore
    result: {
      [table]: {
        computedDurationValue: {
          needs: {
            [operationParameters.columnA]: true,
            [operationParameters.columnB]: true,
          },
          compute: (table) => {
            const dateA = new Date(
              table[operationParameters.columnA]
            ).getTime();
            const dateB = new Date(
              table[operationParameters.columnB]
            ).getTime();
            return Math.abs(dateA - dateB);
          },
        },
      },
    },
  });
  const whereObject = {
    AND: [
      { [operationParameters.columnA]: { not: null } },
      { [operationParameters.columnB]: { not: null } },
      ...(whereAndArray || []),
    ],
  };
  // @ts-ignore
  const prismaQuery: Function = prismaExtended[table]["findMany"];
  const response = await prismaQuery({
    select: {
      computedDurationValue: true,
    },
    where: whereObject,
  });
  const { sum, count } = response.reduce(
    (
      acc: { sum: number; count: number },
      item: { computedDurationValue: null | number }
    ) => {
      if (item.computedDurationValue !== null) {
        acc.sum += item.computedDurationValue;
        acc.count += 1;
      }
      return acc;
    },
    { sum: 0, count: 0 }
  );
  const averageDuration = count > 0 ? sum / count : 0;
  return {
    [`averageDurationBetween${operationParameters.columnA}And${operationParameters.columnB}_ms`]:
      averageDuration,
  };
}
