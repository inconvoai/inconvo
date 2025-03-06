import { type PrismaClient } from "@prisma/client";
import { type Query } from "~/types/querySchema";
import assert from "assert";
import { splitWhereConditions } from "../utils";
import { groupByComputed } from "./computed";
import { groupByJson } from "./json";

export async function groupBy(prisma: PrismaClient, query: Query) {
  assert(query.operation === "groupBy", "Invalid inconvo operation");
  const {
    table,
    operation,
    whereAndArray,
    operationParameters,
    computedColumns,
    jsonColumnSchema,
  } = query;

  const computedColumnNames = computedColumns?.map((column) => column.name);

  // checks everywhere in opParams except the where for the existence of a computed column
  const computedColumnUsed = function (
    opParams: any,
    computedColNames: string[] | undefined
  ) {
    if (!computedColNames) {
      return false;
    }

    const { orderBy, groupBy, sum, min, max } = opParams;
    const opParamColsToCheck = [
      ...(groupBy.map((col: any) => col.column) || []),
      ...(sum?.columns || []),
      ...(min?.columns || []),
      ...(max?.columns || []),
      orderBy?.column,
    ].filter(Boolean);

    return computedColNames.some((str) => opParamColsToCheck.includes(str));
  };

  const [computedWhere, _dbWhere] = splitWhereConditions(
    computedColumns || [],
    whereAndArray
  );

  // computed branch
  if (
    computedColumnUsed(operationParameters, computedColumnNames) ||
    computedWhere.length > 0
  ) {
    return groupByComputed(prisma, query);
  }

  const groupByColumns = operationParameters.groupBy.map(
    (col: any) => col.column
  );
  const countColumns = operationParameters.count?.columns || [];
  const sumColumns = operationParameters.sum?.columns || [];
  const minColumns = operationParameters.min?.columns || [];
  const maxColumns = operationParameters.max?.columns || [];

  const operationColumns: string[] = [
    ...groupByColumns,
    ...countColumns,
    ...sumColumns,
    ...minColumns,
    ...maxColumns,
  ];

  const whereColumns: string[] = whereAndArray
    .map((where) => Object.keys(where))
    .flat();

  const jsoncolumns: string[] = (jsonColumnSchema || []).flatMap((jsonTable) =>
    jsonTable.jsonSchema.map((jsonCol) => jsonCol.name)
  );

  const joinColumns: string[] = operationParameters.groupBy.reduce(
    (acc: string[], column: any) => {
      if (column.join) {
        const tableName = Object.keys(column.join)[0];
        const joinColumn = column.join[tableName];
        acc.push(joinColumn);
      }
      return acc;
    },
    []
  );

  if (
    operationColumns.some((col) => jsoncolumns.includes(col)) ||
    whereColumns.some((col) => jsoncolumns.includes(col)) ||
    joinColumns.some((col) => jsoncolumns.includes(col))
  ) {
    return groupByJson(prisma, query);
  }

  // db branch
  const groupColumns = operationParameters.groupBy.map(
    (col: any) => col.column
  );
  const groupByObject = {
    by: [...groupColumns],
    ...(operationParameters?.count?.columns && {
      _count: operationParameters.count.columns.reduce(
        (acc: Record<string, boolean>, column) => {
          acc[column] = true;
          return acc;
        },
        {}
      ),
    }),
    ...(operationParameters?.sum?.columns && {
      _sum: operationParameters.sum.columns.reduce(
        (acc: Record<string, boolean>, column) => {
          acc[column] = true;
          return acc;
        },
        {}
      ),
    }),
    ...(operationParameters?.min?.columns && {
      _min: operationParameters.min.columns.reduce(
        (acc: Record<string, boolean>, column) => {
          acc[column] = true;
          return acc;
        },
        {}
      ),
    }),
    ...(operationParameters?.max?.columns && {
      _max: operationParameters.max.columns.reduce(
        (acc: Record<string, boolean>, column) => {
          acc[column] = true;
          return acc;
        },
        {}
      ),
    }),
  };

  const groupBy = {
    ...groupByObject,
    [`_${operationParameters.orderBy.function}`]: {
      [operationParameters.orderBy.column]: true,
    },
  };

  const whereObject = {
    AND: [...(whereAndArray || [])],
  };

  assert(
    typeof prisma[table][operation] === "function",
    "Invalid prisma operation"
  );

  const prismaQuery: Function = prisma[table][operation];
  const response = await prismaQuery({
    ...groupBy,
    where: whereObject,
    orderBy: {
      [`_${operationParameters.orderBy.function}`]: {
        [operationParameters.orderBy.column]:
          operationParameters.orderBy.direction,
      },
    },
    take: operationParameters.limit,
  });

  if (
    operationParameters.groupBy.some((groupColumn: any) => groupColumn.join)
  ) {
    for (const column of operationParameters.groupBy) {
      if (column.join) {
        const tableName = Object.keys(column.join)[0];
        const joinColumn = column.join[tableName];
        const joins = await prisma[table]["findMany"]({
          select: {
            [column.column]: true,
            [tableName]: {
              select: {
                [joinColumn]: true,
              },
            },
          },
          where: {
            [column.column]: {
              in: response.map((row: any) => row[column.column]),
            },
          },
          distinct: [column.column],
        });

        response.forEach((row: any) => {
          const relation = joins.find(
            (join: any) => join[column.column] === row[column.column]
          );
          row[joinColumn] = relation[tableName][joinColumn];
        });
      }
    }
  }

  return response;
}
