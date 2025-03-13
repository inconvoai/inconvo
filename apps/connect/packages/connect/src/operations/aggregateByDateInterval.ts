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
import { isPrismaFieldNullable } from "~/util/isPrismaFieldNullable";

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

export async function aggregateByDateInterval(
  prisma: PrismaClient,
  query: Query
) {
  assert(
    query.operation === "aggregateByDateInterval",
    "Invalid inconvo operation"
  );
  const { table, whereAndArray, operationParameters, computedColumns } = query;
  const { dateColumn, interval, aggregationType, aggregateColumn } =
    operationParameters;

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
          needs: { [dateColumn]: true },
          compute: (table: any) => {
            const date = new Date(table[dateColumn]);
            const formattedDate = formatDateByInterval(date, interval);
            return formattedDate;
          },
        },
      },
    },
  });

  const isNullable = isPrismaFieldNullable(dateColumn, table);

  const whereObject = {
    AND: [
      ...(isNullable ? [{ [dateColumn]: { not: null } }] : []),
      // For non-count aggregations where a field is needed, ensure that field isn't null
      ...(aggregationType !== "count" &&
      aggregateColumn &&
      isPrismaFieldNullable(aggregateColumn, table)
        ? [{ [aggregateColumn]: { not: null } }]
        : []),
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

  // Select the field to aggregate if it's not a computed column
  const selectFields = {
    computedDateInterval: true,
    ...(aggregationType !== "count" &&
    aggregateColumn &&
    !selectComputedCols[aggregateColumn]
      ? { [aggregateColumn]: true }
      : {}),
    ...selectComputedCols,
  };

  // @ts-expect-error
  const prismaQuery: Function = prismaExtended[table]["findMany"];
  const response = await prismaQuery({
    select: selectFields,
    where: whereObject,
  });

  const resWithWhereComputedCondition = filterResponseWithComputedConditions(
    response,
    computedWhere
  );

  // Group by date interval and perform aggregation
  const aggregationMap = new Map<string, any[]>();
  resWithWhereComputedCondition.forEach((row: any) => {
    const date = row.computedDateInterval;
    if (!aggregationMap.has(date)) {
      aggregationMap.set(date, []);
    }

    if (aggregationType === "count") {
      aggregationMap.get(date)?.push(1);
    } else if (aggregateColumn) {
      const value = row[aggregateColumn];
      if (value !== null && value !== undefined) {
        aggregationMap.get(date)?.push(value);
      }
    }
  });

  // Apply aggregation based on the type
  const aggregatedResults: Record<string, unknown> = {};
  aggregationMap.forEach((values, date) => {
    if (values.length === 0) return;

    switch (aggregationType) {
      case "min":
        aggregatedResults[date] = { min: Math.min(...values) };
        break;
      case "max":
        aggregatedResults[date] = { max: Math.max(...values) };
        break;
      case "count":
        aggregatedResults[date] = { count: values.length };
        break;
      case "sum":
        aggregatedResults[date] = {
          sum: values.reduce((sum, val) => sum + (Number(val) || 0), 0),
        };
        break;
      case "avg":
        aggregatedResults[date] = {
          avg:
            values.reduce((sum, val) => sum + (Number(val) || 0), 0) /
            values.length,
        };
        break;
    }
  });

  // Sort results by date
  return Object.fromEntries(
    Object.entries(aggregatedResults).sort(
      ([a], [b]) => new Date(a).getTime() - new Date(b).getTime()
    )
  );
}
