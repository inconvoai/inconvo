import assert from "assert";
import { type Query } from "~/types/querySchema";
import { buildRelationalSelect } from "~/util/buildRelationalSelect";
import { type PrismaComputedColumnClient } from "~/util/generatePrismaClientWithComputedColumns";
import {
  filterResponseWithComputedConditions,
  splitWhereConditions,
} from "../utils";
import { Prisma } from "@prisma/client";

export interface OrderedResponse {
  [key: string]: any;
}

export async function computedFindMany(
  prisma: PrismaComputedColumnClient,
  query: Query
) {
  assert(query.operation === "findMany", "Invalid inconvo operation");
  const { table, whereAndArray, operationParameters, computedColumns } = query;

  const [computedWhere, dbWhere] = splitWhereConditions(
    computedColumns || [],
    whereAndArray
  );

  const computedColumnNames = computedColumns?.map((column) => column.name);

  const computedWhereColumnNames =
    computedWhere.length > 0
      ? computedWhere.map((condition) => Object.keys(condition)).flat()
      : [];

  const orderByColumnIsComputed =
    operationParameters.orderBy &&
    computedColumnNames?.includes(operationParameters.orderBy.column);

  const injectedComputed = {
    ...(orderByColumnIsComputed &&
      operationParameters.orderBy && {
        [operationParameters.orderBy.column]: true,
      }),
    ...computedWhereColumnNames.reduce(
      (acc, col) => ({ ...acc, [col]: true }),
      {}
    ),
  };

  const select = buildRelationalSelect(table, operationParameters.columns);

  const modifiedSelect = {
    ...select,
    ...injectedComputed,
  };

  // handle where computed field is in at least 1 of where or orderby.
  // in this case select without join, where, orderby, take then join
  const model = Prisma.dmmf.datamodel.models.find(
    (model) => model.name === table
  );

  // grab a unique key from the table to join on later
  const uniqueKey = model
    ? // FIXME: doesn't handle multiple unique keys
      model.fields.filter((field) => field.isId).map((field) => field.name)[0]
    : undefined;
  assert(uniqueKey, "No unique key found for table");

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

  const [nonRelationalSelect, relationalSelect] = splitSelect(modifiedSelect);

  // NOTE: Can never apply a limit on this as it will break the order.
  // @ts-expect-error - We don't know the table name in advance
  const prismaQuery: Function = prisma[table]["findMany"];
  const response = await prismaQuery({
    select: {
      ...nonRelationalSelect, // select the non relational columns
      [uniqueKey]: true, // add the unique key to the response so we can join on it later
    },
    where: { AND: [...(dbWhere || [])] }, // apply only db field where conditions
    ...(!orderByColumnIsComputed && {
      orderBy: operationParameters.orderBy
        ? {
            [operationParameters.orderBy.column]:
              operationParameters.orderBy.direction,
          }
        : undefined, // if the order by isn't computed apply it now
    }),
  });

  // if computed where - apply it
  const filteredRes =
    computedWhere.length > 0
      ? filterResponseWithComputedConditions(response, computedWhere)
      : response;

  // if computed orderby - apply it
  const orderedRes: OrderedResponse[] = orderByColumnIsComputed
    ? operationParameters.orderBy?.direction === "asc"
      ? filteredRes.toSorted((a: OrderedResponse, b: OrderedResponse) =>
          operationParameters.orderBy
            ? a[operationParameters.orderBy.column] -
              b[operationParameters.orderBy.column]
            : 0
        )
      : filteredRes.toSorted((a: OrderedResponse, b: OrderedResponse) =>
          operationParameters.orderBy
            ? b[operationParameters.orderBy.column] -
              a[operationParameters.orderBy.column]
            : 0
        )
    : filteredRes;

  // if computed where apply the limit
  const limitedRes = orderedRes.slice(0, operationParameters.limit);

  // @ts-expect-error
  const joins = await prisma[table].findMany({
    where: {
      [uniqueKey]: {
        in: limitedRes.map((row) => row[uniqueKey]),
      },
    },
    select: { ...relationalSelect, [uniqueKey]: true },
  });

  limitedRes.forEach((row) => {
    const relation: Record<string, any> | undefined = joins.find(
      (join: Record<string, any>) => join[uniqueKey] === row[uniqueKey]
    );
    Object.assign(row, relation);
    delete row[uniqueKey];
  });

  return limitedRes;
}

export async function selectOnlyComputedFindMany(
  prisma: PrismaComputedColumnClient,
  query: Query
) {
  assert(query.operation === "findMany", "Invalid inconvo operation");
  const { table, whereAndArray, operationParameters, computedColumns } = query;
  const select = buildRelationalSelect(table, operationParameters.columns);
  const [computedWhere, dbWhere] = splitWhereConditions(
    computedColumns || [],
    whereAndArray
  );
  // @ts-expect-error
  const prismaQuery: Function = prisma[table]["findMany"];
  const response = await prismaQuery({
    select,
    where: { AND: [...(dbWhere || [])] },
    take: operationParameters.limit,
    orderBy: operationParameters.orderBy
      ? {
          [operationParameters.orderBy.column]:
            operationParameters.orderBy.direction,
        }
      : undefined,
  });

  return response;
}
