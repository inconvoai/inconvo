import { type PrismaClient } from "@prisma/client";
import { type Query } from "~/types/querySchema";
import assert from "assert";
import { findDistinctJson } from "./json";

export async function findDistinct(prisma: PrismaClient, query: Query) {
  assert(query.operation === "findDistinct", "Invalid inconvo operation");
  const { table, whereAndArray, operationParameters, jsonColumnSchema } = query;

  const jsonSchemaMapForTable = jsonColumnSchema?.find(
    (jsonCol) => jsonCol.tableName === table
  );

  const jsonColumnNames =
    jsonSchemaMapForTable?.jsonSchema.map((col) => col.name) || [];

  if (jsonColumnNames.includes(operationParameters.column)) {
    return findDistinctJson(prisma, query);
  }

  const whereObject = {
    AND: [...(whereAndArray || [])],
  };
  // @ts-ignore
  const prismaQuery: Function = prisma[table]["findMany"];
  const response = await prismaQuery({
    select: {
      [operationParameters.column]: true,
    },
    distinct: [operationParameters.column],
    where: whereObject,
  });
  return response;
}
