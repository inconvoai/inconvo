import { type PrismaClient } from "@prisma/client";
import { type Query } from "~/types/querySchema";
import assert from "assert";
import { generatePrismaClientWithComputedColumns } from "~/util/generatePrismaClientWithComputedColumns";
import {
  filterResponseWithComputedConditions,
  splitWhereConditions,
} from "../utils";

export async function groupByComputed(prisma: PrismaClient, query: Query) {
  assert(query.operation === "groupBy", "Invalid inconvo operation");
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

  const xPrisma = generatePrismaClientWithComputedColumns(
    prisma,
    table,
    computedColumns
  );

  const selectColumns = (opParams: any) => {
    const { limit, orderBy, groupBy, ...metricColumns } = opParams;
    const allColumnNames = Object.keys(metricColumns).reduce(
      (acc: { [key: string]: any }, column: string) => {
        if (metricColumns[column] && metricColumns[column].columns) {
          metricColumns[column].columns.forEach((col: string) => {
            acc[col] = true;
          });
        }
        return acc;
      },
      {}
    );

    groupBy.forEach((col: any) => {
      allColumnNames[col.column] = true;
    });

    // take out the join key and add that key: col : true
    groupBy.forEach((col: any) => {
      if (col.join) {
        const tableName = Object.keys(col.join)[0];
        const joinColumn = col.join[tableName];
        if (!allColumnNames[tableName]) {
          allColumnNames[tableName] = {};
          allColumnNames[tableName].select = {};
        }
        allColumnNames[tableName]["select"][joinColumn] = true;
      }
    });

    allColumnNames[orderBy.column] = true;

    return allColumnNames;
  };

  function splitSelect(
    obj: Record<string, any>
  ): [Record<string, boolean>, Record<string, any>] {
    return Object.keys(obj).reduce(
      (
        acc: [Record<string, boolean>, Record<string, any>],
        key: string
      ): [Record<string, boolean>, Record<string, any>] => {
        if (typeof obj[key] !== "object" || obj[key] === null) {
          acc[0][key] = obj[key];
        } else {
          acc[1][key] = obj[key];
        }
        return acc;
      },
      [{} as Record<string, boolean>, {} as Record<string, any>]
    );
  }

  const columnsForSelection = selectColumns(operationParameters);
  const [nonRelationalSelect, relationalSelect] =
    splitSelect(columnsForSelection);

  // @ts-expect-error - We don't know the table name in advance
  const prismaQuery: Function = xPrisma[table]["findMany"];
  const response = await prismaQuery({
    select: nonRelationalSelect,
    where: {
      AND: [...(dbWhere || [])],
    },
  });

  const resWithWhereComputedCondition =
    computedWhere.length > 0
      ? filterResponseWithComputedConditions(response, computedWhere)
      : response;

  // FIXME: This is a temporary implementation just groups by 1 column
  const groupByColumn = operationParameters.groupBy[0];

  const groupedByValues = resWithWhereComputedCondition.reduce(
    (acc: any, row: any) => {
      const { [groupByColumn.column]: groupByValue, ...rest } = row;
      if (!acc[groupByValue]) {
        acc[groupByValue] = {};
      }

      // Ensure the groupBy column itself is included in aggregations
      if (!acc[groupByValue][groupByColumn.column]) {
        acc[groupByValue][groupByColumn.column] = [];
      }
      acc[groupByValue][groupByColumn.column].push(groupByValue);

      Object.keys(rest).forEach((key) => {
        if (!acc[groupByValue][key]) {
          acc[groupByValue][key] = [];
        }
        acc[groupByValue][key].push(rest[key]);
      });
      return acc;
    },
    {}
  );

  const groupedByResults: {
    _min?: { [key: string]: number };
    _max?: { [key: string]: number };
    _count?: { [key: string]: number };
    _sum?: { [key: string]: number };
    _avg?: { [key: string]: number };
  }[] = [];

  Object.keys(groupedByValues).forEach((groupedByValue) => {
    const group: {
      _min?: { [key: string]: number };
      _max?: { [key: string]: number };
      _count?: { [key: string]: number };
      _sum?: { [key: string]: number };
      _avg?: { [key: string]: number };
    } = {
      [groupByColumn.column]: groupedByValue,
    };

    // Process the aggregation functions based on the requested columns
    if (operationParameters.min && operationParameters.min.columns) {
      group["_min"] = {};
      operationParameters.min.columns.forEach((columnName: string) => {
        if (groupedByValues[groupedByValue][columnName]) {
          group["_min"]![columnName] = Math.min(
            ...groupedByValues[groupedByValue][columnName]
          );
        }
      });
    }

    if (operationParameters.max && operationParameters.max.columns) {
      group["_max"] = {};
      operationParameters.max.columns.forEach((columnName: string) => {
        if (groupedByValues[groupedByValue][columnName]) {
          group["_max"]![columnName] = Math.max(
            ...groupedByValues[groupedByValue][columnName]
          );
        }
      });
    }

    if (operationParameters.sum && operationParameters.sum.columns) {
      group["_sum"] = {};
      operationParameters.sum.columns.forEach((columnName: string) => {
        if (groupedByValues[groupedByValue][columnName]) {
          group["_sum"]![columnName] = groupedByValues[groupedByValue][
            columnName
          ].reduce((acc: number, curr: number) => acc + Number(curr), 0);
        }
      });
    }

    if (operationParameters.count && operationParameters.count.columns) {
      group["_count"] = {};
      operationParameters.count.columns.forEach((columnName: string) => {
        if (groupedByValues[groupedByValue][columnName]) {
          group["_count"]![columnName] =
            groupedByValues[groupedByValue][columnName].length;
        }
      });
    }

    if (operationParameters.avg && operationParameters.avg.columns) {
      group["_avg"] = {};
      operationParameters.avg.columns.forEach((columnName: string) => {
        if (groupedByValues[groupedByValue][columnName]) {
          const values = groupedByValues[groupedByValue][columnName];
          group["_avg"]![columnName] =
            values.reduce(
              (acc: number, curr: number) => acc + Number(curr),
              0
            ) / values.length;
        }
      });
    }

    groupedByResults.push(group);
  });

  if (operationParameters.orderBy) {
    const { direction, function: func, column } = operationParameters.orderBy;
    groupedByResults.sort((a, b) => {
      const aValue = a[`_${func}`]?.[column] ?? 0;
      const bValue = b[`_${func}`]?.[column] ?? 0;
      return direction === "asc" ? aValue - bValue : bValue - aValue;
    });
  }

  const limitedRes = groupedByResults.slice(0, operationParameters.limit);

  if (
    operationParameters.groupBy.some((groupColumn: any) => groupColumn.join)
  ) {
    for (const column of operationParameters.groupBy) {
      if (column.join) {
        const tableName = Object.keys(column.join)[0];
        const joinColumn = column.join[tableName];
        // @ts-expect-error
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
              in: limitedRes.map((row: any) => row[column.column]),
            },
          },
          distinct: [column.column],
        });

        limitedRes.forEach((row: any) => {
          const relation = joins.find(
            (join: any) => join[column.column] === row[column.column]
          );
          row[joinColumn] = relation[tableName][joinColumn];
        });
      }
    }
  }

  return limitedRes;
}
