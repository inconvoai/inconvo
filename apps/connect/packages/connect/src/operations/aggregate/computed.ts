import { type PrismaClient } from "@prisma/client";
import { type Query } from "~/types/querySchema";
import { generatePrismaClientWithComputedColumns } from "~/util/generatePrismaClientWithComputedColumns";
import {
  filterResponseWithComputedConditions,
  getSelectColumns,
  splitWhereConditions,
} from "../utils";
import assert from "assert";

export async function aggregateWithComputedColumn(
  prisma: PrismaClient,
  query: Query
) {
  assert(query.operation === "aggregate", "Invalid inconvo operation");
  const { table, whereAndArray, operationParameters, computedColumns } = query;

  const [computedWhere, dbWhere] = splitWhereConditions(
    computedColumns || [],
    whereAndArray
  );

  const whereObject = {
    AND: [...(dbWhere || [])],
  };

  const xPrisma = generatePrismaClientWithComputedColumns(
    prisma,
    table,
    computedColumns
  );

  const columnNames = [
    ...(operationParameters.avg ?? []),
    ...(operationParameters.sum ?? []),
    ...(operationParameters.min ?? []),
    ...(operationParameters.max ?? []),
    ...(operationParameters.count ?? []),
    ...(operationParameters.median ?? []),
  ];

  const selectColumns = getSelectColumns(columnNames, computedColumns);

  // @ts-expect-error - We don't know the table name in advance
  const prismaQuery: Function = xPrisma[table]["findMany"];
  const response = await prismaQuery({
    select: selectColumns,
    where: whereObject,
  });

  // apply any computed where logic here
  const resWithWhereComputedCondition = filterResponseWithComputedConditions(
    response,
    computedWhere
  );

  const metrics: {
    _avg: { [key: string]: number | null };
    _sum: { [key: string]: number };
    _min: { [key: string]: number | null };
    _max: { [key: string]: number | null };
    _count: { [key: string]: number };
    _median: { [key: string]: number | null };
  } = {
    _avg: {},
    _sum: {},
    _min: {},
    _max: {},
    _count: {},
    _median: {},
  };

  // Process each metric type separately
  const processMetrics = (
    columns: string[] | null | undefined,
    metricType: keyof typeof metrics
  ) => {
    if (!columns) return;

    columns.forEach((column: string) => {
      const rawValues = resWithWhereComputedCondition.map(
        (item: Record<string, unknown>) => item[column]
      );

      // Type guard to ensure we only work with numbers
      const values = rawValues.filter(
        (val): val is number => typeof val === "number" && !isNaN(val)
      );

      if (values.length === 0) {
        if (metricType !== "_count" && metricType !== "_sum") {
          metrics[metricType][column] = null;
        } else if (metricType === "_count") {
          metrics._count[column] = 0;
        } else {
          metrics._sum[column] = 0;
        }
        return;
      }

      switch (metricType) {
        case "_min":
          metrics._min[column] = Math.min(...values);
          break;
        case "_max":
          metrics._max[column] = Math.max(...values);
          break;
        case "_count":
          metrics._count[column] = values.length;
          break;
        case "_sum":
          metrics._sum[column] = values.reduce((acc, curr) => acc + curr, 0);
          break;
        case "_avg":
          const sum = values.reduce((acc, curr) => acc + curr, 0);
          metrics._avg[column] = sum / values.length;
          break;
        case "_median":
          // Sort values and find the middle element(s)
          const sortedValues = [...values].sort((a, b) => a - b);
          const mid = Math.floor(sortedValues.length / 2);
          metrics._median[column] =
            sortedValues.length % 2 === 0
              ? (sortedValues[mid - 1] + sortedValues[mid]) / 2
              : sortedValues[mid];
          break;
      }
    });
  };

  processMetrics(operationParameters.min, "_min");
  processMetrics(operationParameters.max, "_max");
  processMetrics(operationParameters.avg, "_avg");
  processMetrics(operationParameters.sum, "_sum");
  processMetrics(operationParameters.count, "_count");
  processMetrics(operationParameters.median, "_median");

  // Return only the metrics that were requested
  const filteredMetrics = Object.entries(metrics).reduce(
    (acc: any, [key, value]) => {
      const metricType = key.slice(1) as keyof typeof operationParameters;
      if (operationParameters[metricType]) {
        acc[key] = value;
      }
      return acc;
    },
    {} as typeof metrics
  );

  return filteredMetrics;
}
