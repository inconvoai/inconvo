import { type PrismaClient } from "@prisma/client";
import { type Query } from "~/types/querySchema";
import { format, startOfMonth, startOfDay } from "date-fns";
import assert from "assert";
import {
  filterResponseWithComputedConditions,
  splitWhereConditions,
} from "./utils";
import { generatePrismaClientWithComputedColumns } from "~/util/generatePrismaClientWithComputedColumns";
import { isPrismaFieldNullable } from "~/util/isPrismaFieldNullable";

type temporalComponent = "Day" | "Month";
function formatDateToTemporalComponent(
  date: Date,
  temporalComponent: temporalComponent
): string {
  let formattedDate: string;
  switch (temporalComponent) {
    case "Day":
      formattedDate = format(startOfDay(date), "EEEE");
      break;
    case "Month":
      formattedDate = format(startOfMonth(date), "MMMM");
      break;
    default:
      throw new Error("Invalid interval");
  }
  return formattedDate;
}

export async function countByTemporalComponent(
  prisma: PrismaClient,
  query: Query
) {
  assert(
    query.operation === "countByTemporalComponent",
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
    // @ts-expect-error - We don't know the table name in advance
    result: {
      [`${table.charAt(0).toLowerCase() + table.slice(1)}`]: {
        computedTemporalComponent: {
          needs: { [operationParameters.dateColumn]: true },
          compute: (table) => {
            const date = new Date(table[operationParameters.dateColumn]);
            const formattedDate = formatDateToTemporalComponent(
              date,
              operationParameters.component
            );
            return formattedDate;
          },
        },
      },
    },
  });

  const isNullable = isPrismaFieldNullable(
    operationParameters.dateColumn,
    table
  );

  const whereObject = {
    AND: [
      ...(isNullable
        ? [{ [operationParameters.dateColumn]: { not: null } }]
        : []),
      ...(dbWhere || []),
    ],
  };

  const selectComputedCols = computedWhere.reduce<Record<string, boolean>>(
    (acc, obj) => {
      const key = Object.keys(obj)[0];
      acc[key] = true;
      return acc;
    },
    {}
  );

  // @ts-expect-error - We don't know the table name in advance
  const prismaQuery: Function = prismaExtended[table]["findMany"];
  const response = await prismaQuery({
    select: {
      computedTemporalComponent: true,
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
    (row: { computedTemporalComponent: string }) => {
      const date = row.computedTemporalComponent;
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
