import { type PrismaClient } from "@prisma/client";
import { type Query } from "~/types/querySchema";
import assert from "assert";
import { getSelectColumns, splitWhereConditions } from "~/operations/utils";
import { countRelationsComputed } from "./computed";
import { countRelationsJson } from "./json";
import { env } from "~/env";

export async function countRelations(prisma: PrismaClient, query: Query) {
  assert(query.operation === "countRelations", "Invalid inconvo operation");
  const {
    table,
    whereAndArray,
    operationParameters,
    computedColumns,
    jsonColumnSchema,
  } = query;

  const columns = operationParameters.columns;
  const jsonSchemaForTable = jsonColumnSchema?.find(
    (jsonCol) => jsonCol.tableName === table
  );
  const jsonColumns =
    jsonSchemaForTable?.jsonSchema.map((col) => col.name) || [];

  if (
    columns.some((column) => jsonColumns.includes(column)) &&
    env.DRIZZLE === "TRUE"
  ) {
    return countRelationsJson(prisma, query);
  }

  const [computedWhere, dbWhere] = splitWhereConditions(
    computedColumns || [],
    whereAndArray
  );

  if (computedWhere.length > 0) {
    return countRelationsComputed(prisma, query);
  }

  const whereObject = {
    AND: [...(whereAndArray || [])],
  };

  const selectColumns = getSelectColumns(
    operationParameters.columns,
    computedColumns
  );

  const countObject = operationParameters.relationsToCount
    ? operationParameters?.relationsToCount.reduce(
        (acc: Record<string, boolean>, relationObject) => {
          acc[relationObject.name] = true;
          return acc;
        },
        {}
      )
    : undefined;

  const needDistinct = operationParameters.relationsToCount.some(
    (relation) => relation.distinct
  );

  if (!needDistinct) {
    // @ts-expect-error  - We don't know the table name in advance
    const prismaQuery: Function = prisma[table]["findMany"];
    const response = await prismaQuery({
      select: {
        ...selectColumns,
        _count: {
          select: countObject,
        },
      },
      where: whereObject,
      orderBy: operationParameters.orderBy
        ? {
            [operationParameters.orderBy.relation]: {
              _count: operationParameters.orderBy.direction,
            },
          }
        : undefined,
      take: operationParameters.limit,
    });
    return response.length > 0 ? response : 0;
  }

  const relationSelect = operationParameters.relationsToCount.reduce(
    (acc: Record<string, any>, relation) => {
      if (relation.distinct) {
        acc[relation.name] = {
          select: {
            [relation.distinct]: true,
          },
          distinct: [relation.distinct],
        };
      } else {
        acc._count = {
          select: {
            [relation.name]: true,
          },
        };
      }
      return acc;
    },
    {}
  );

  // @ts-expect-error  - We don't know the table name in advance
  const prismaQuery: Function = prisma[table]["findMany"];
  const initialData = await prismaQuery({
    select: {
      ...selectColumns,
      ...relationSelect,
    },
    where: whereObject,
  });

  const dataWithCounts = initialData.map((item: any) => {
    const itemKeys = Object.keys(item);
    // Create a map of relation names to whether they use distinct
    const distinctRelations = operationParameters.relationsToCount.reduce(
      (map: Record<string, string | undefined>, rel) => {
        if (rel.distinct) {
          map[rel.name] = rel.distinct;
        }
        return map;
      },
      {}
    );

    const withCounts = itemKeys.reduce(
      (acc: Record<string, any>, key: string) => {
        if (operationParameters.columns.some((column) => column === key)) {
          // These are regular columns to select
          acc[key] = item[key];
        } else if (key === "_count") {
          // These are regular counts (non-distinct)
          acc["_count"] = item["_count"];
        } else {
          // These are relations with distinct counts
          if (!acc["_count"]) {
            acc["_count"] = {};
          }
          const distinctField = distinctRelations[key];
          if (distinctField) {
            // For distinct counts, we're counting unique values of a specific field
            acc["_count"][key] = item[key].length ?? 0;
          }
        }
        return acc;
      },
      {}
    );
    return withCounts;
  });

  function sortByRelationCount(
    data: any[],
    relation: string,
    direction: "asc" | "desc",
    limit?: number
  ): any[] {
    // Sort the data based on the relation count
    const sortedData = [...data].sort((a, b) => {
      const countA = a._count?.[relation] ?? 0;
      const countB = b._count?.[relation] ?? 0;

      return direction === "asc" ? countA - countB : countB - countA;
    });

    // Apply limit if provided
    return limit ? sortedData.slice(0, limit) : sortedData;
  }

  let result = dataWithCounts;

  if (operationParameters.orderBy) {
    const { relation, direction } = operationParameters.orderBy;
    result = sortByRelationCount(
      dataWithCounts,
      relation,
      direction,
      operationParameters.limit
    );
  }

  return result.length > 0 ? result : 0;
}
