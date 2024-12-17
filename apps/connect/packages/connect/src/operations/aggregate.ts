import { type PrismaClient } from "@prisma/client";
import { type Query } from "~/types/querySchema";
import assert from "assert";
import { generatePrismaClientWithComputedColumns } from "~/util/generatePrismaClientWithComputedColumns";
import {
  filterResponseWithComputedConditions,
  getSelectColumns,
  splitWhereConditions,
} from "./utils";

export async function aggregate(prisma: PrismaClient, query: Query) {
  assert(query.operation === "aggregate", "Invalid inconvo operation");
  const {
    table,
    operation,
    whereAndArray,
    operationParameters,
    computedColumns,
  } = query;

  const [computedWhere, dbWhere] = splitWhereConditions(
    computedColumns || [],
    whereAndArray
  );

  const whereObject = {
    AND: [...(dbWhere || [])],
  };

  const columns = operationParameters.columns.reduce(
    (acc: { [key: string]: boolean }, column: string) => {
      acc[column] = true;
      return acc;
    },
    {}
  );

  const computedColumnNames = computedColumns?.map((column) => column.name);
  const computedColumnUsed = function (
    opParams: any,
    computedColNames: string[] | undefined
  ) {
    if (!computedColNames) {
      return false;
    }

    const { columns } = opParams;
    return columns.some((col: string) => computedColNames.includes(col));
  };

  if (
    computedColumnUsed(operationParameters, computedColumnNames) ||
    computedWhere.length > 0
  ) {
    const xPrisma = generatePrismaClientWithComputedColumns(
      prisma,
      table,
      computedColumns
    );

    const selectColumns = getSelectColumns(
      operationParameters.columns,
      computedColumns
    );

    //@ts-ignore
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
      _avg: { [key: string]: number };
      _sum: { [key: string]: number };
      _min: { [key: string]: number };
      _max: { [key: string]: number };
      _count: { [key: string]: number };
    } = {
      _avg: {},
      _sum: {},
      _min: {},
      _max: {},
      _count: {},
    };

    operationParameters.columns.forEach((column: string) => {
      const result = resWithWhereComputedCondition.map(
        (item: any) => item[column]
      );
      const min = Math.min(...result);
      const max = Math.max(...result);
      const count = result.length;
      const sum = result.reduce((acc: any, curr: any) => acc + curr, 0);
      const avg = sum / count;

      metrics._avg[column] = avg;
      metrics._sum[column] = sum;
      metrics._min[column] = min;
      metrics._max[column] = max;
      metrics._count[column] = count;
    });

    return metrics;
  }
  assert(
    // @ts-ignore
    typeof prisma[table][operation] === "function",
    "Invalid prisma operation"
  );
  // @ts-ignore
  const prismaQuery: Function = prisma[table][operation];

  const response = await prismaQuery({
    _avg: { ...columns },
    _sum: { ...columns },
    _min: { ...columns },
    _max: { ...columns },
    _count: { ...columns },
    where: { AND: [...(whereAndArray || [])] },
  });
  return response;
}
