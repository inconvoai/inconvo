import { type PrismaClient } from "@prisma/client";
import { type Query } from "~/types/querySchema";
import {
  format,
  startOfYear,
  startOfMonth,
  startOfWeek,
  startOfDay,
} from "date-fns";
import assert from "assert";
import { generatePrismaClientWithComputedColumns } from "~/util/generatePrismaClientWithComputedColumns";
import {
  filterResponseWithComputedConditions,
  splitWhereConditions,
} from "./utils";

type interval = "year" | "month" | "week" | "day";
function formatDateByInterval(date: Date, interval: interval): string {
  let formattedDate: string;
  switch (interval) {
    case "year":
      formattedDate = format(startOfYear(date), "yyyy-MM-dd");
      break;
    case "month":
      formattedDate = format(startOfMonth(date), "yyyy-MM-dd");
      break;
    case "week":
      formattedDate = format(startOfWeek(date), "yyyy-MM-dd");
      break;
    case "day":
      formattedDate = format(startOfDay(date), "yyyy-MM-dd");
      break;
    default:
      throw new Error("Invalid interval");
  }
  return formattedDate;
}

export async function countByDateInterval(prisma: PrismaClient, query: Query) {
  assert(
    query.operation === "countByDateInterval",
    "Invalid inconvo operation"
  );
  const { table, whereAndArray, operationParameters, computedColumns } = query;

  const [computedWhere, dbWhere] = splitWhereConditions(
    computedColumns || [],
    whereAndArray
  );

  const prismaClient =
    computedWhere.length > 0
      ? generatePrismaClientWithComputedColumns(prisma, table, computedColumns)
      : prisma;

  const prismaExtended = prismaClient.$extends({
    // @ts-expect-error
    result: {
      [`${table.charAt(0).toLowerCase() + table.slice(1)}`]: {
        computedDateInterval: {
          needs: { [operationParameters.dateColumn]: true },
          compute: (table) => {
            const date = new Date(table[operationParameters.dateColumn]);
            const formattedDate = formatDateByInterval(
              date,
              operationParameters.interval
            );
            return formattedDate;
          },
        },
      },
    },
  });

  const whereObject = {
    AND: [
      { [operationParameters.dateColumn]: { not: null } },
      ...(dbWhere || []),
    ],
  };

  const selectComputedCols = computedWhere.reduce(
    (acc: Record<string, boolean>, obj) => {
      const key = Object.keys(obj)[0];
      acc[key] = true;
      return acc;
    },
    {}
  );

  // @ts-expect-error
  const prismaQuery: Function = prismaExtended[table]["findMany"];
  const response = await prismaQuery({
    select: {
      computedDateInterval: true,
      ...selectComputedCols,
    },
    where: whereObject,
  });

  const resWithWhereComputedCondition = filterResponseWithComputedConditions(
    response,
    computedWhere
  );

  const countByColumnMap = new Map<string, number>();
  resWithWhereComputedCondition.forEach(
    (row: { computedDateInterval: string }) => {
      const date = row.computedDateInterval;
      countByColumnMap.set(date, (countByColumnMap.get(date) || 0) + 1);
    }
  );
  const orderedCountByColumn = Array.from(countByColumnMap.entries())
    .sort((a, b) => new Date(a[0]).getTime() - new Date(b[0]).getTime())
    .reduce((acc: { [x: string]: number }, [key, value]) => {
      acc[key] = value;
      return acc;
    }, {});
  return orderedCountByColumn;
}
