import { type Query } from "~/types/querySchema";
import {
  asc,
  avg,
  count,
  desc,
  eq,
  getTableColumns,
  max,
  min,
  sql,
  sum,
  Table,
  WithSubquery,
} from "drizzle-orm";
import { parsePrismaWhere } from "~/operations/utils/prismaToDrizzleWhereConditions";
import { findRelationsBetweenTables } from "~/operations/utils/findRelationsBetweenTables";
import { loadDrizzleSchema } from "~/util/loadDrizzleSchema";
import assert from "assert";

export async function groupBy(db: any, query: Query) {
  assert(query.operation === "groupBy", "Invalid inconvo operation");
  const { table, whereAndArray, operationParameters, jsonColumnSchema } = query;

  const tables = await loadDrizzleSchema();

  const jsonSchemaForTable = jsonColumnSchema?.find(
    (jsonCol) => jsonCol.tableName === table
  );
  const jsonCols = jsonSchemaForTable?.jsonSchema.map((col) => col.name) || [];
  const jsonColumnName = jsonSchemaForTable?.jsonColumnName;

  const tableAlias = db.$with(`${table}Alias`).as(
    db
      .select({
        ...getTableColumns(tables[table]),
        ...jsonCols.reduce((acc: Record<string, unknown>, col) => {
          acc[col] = sql
            .raw(
              `cast((${jsonColumnName}->>'${col}') as ${
                jsonSchemaForTable?.jsonSchema.find((jCol) => jCol.name === col)
                  ?.type === "String"
                  ? "Text"
                  : "Numeric"
              })`
            )
            .as(col);
          return acc;
        }, {}),
      })
      .from(tables[table])
  );

  const tableAliasMapper: Record<string, WithSubquery> = {};
  const tableAliases: WithSubquery[] = [];
  const tablesToAlias =
    jsonColumnSchema
      ?.map((jsonCol) => jsonCol.tableName)
      .filter((t) => t !== table) || [];
  for (const table of tablesToAlias) {
    const jsonSchemaForTable = jsonColumnSchema?.find(
      (jsonCol) => jsonCol.tableName === table
    );
    const jsonCols =
      jsonSchemaForTable?.jsonSchema.map((col) => col.name) || [];
    if (jsonCols.length === 0) {
      continue;
    }
    const jsonColumnName = jsonSchemaForTable?.jsonColumnName;

    const tableAlias = db.$with(`${table}Alias`).as(
      db
        .select({
          ...getTableColumns(tables[table]),
          ...jsonCols.reduce((acc: Record<string, unknown>, col) => {
            acc[col] = sql
              .raw(
                `cast((${jsonColumnName}->>'${col}') as ${
                  jsonSchemaForTable?.jsonSchema.find(
                    (jCol) => jCol.name === col
                  )?.type === "String"
                    ? "Text"
                    : "Numeric"
                })`
              )
              .as(col);
            return acc;
          }, {}),
        })
        .from(tables[table])
    );
    tableAliases.push(tableAlias);
    tableAliasMapper[table] = tableAlias;
  }

  const countJsonFields = operationParameters.count?.columns.map(
    (col) => sql`${col}::text, ${count(tableAlias[col])}`
  );
  const minJsonFields = operationParameters.min?.columns.map(
    (col) => sql`${col}::text, ${min(tableAlias[col])}`
  );
  const maxJsonFields = operationParameters.max?.columns.map(
    (col) => sql`${col}::text, ${max(tableAlias[col])}`
  );
  const sumJsonFields = operationParameters.sum?.columns.map(
    (col) => sql`${col}::text,${sum(tableAlias[col])}`
  );
  const avgJsonFields = operationParameters.avg?.columns.map(
    (col) => sql`${col}::text,${avg(tableAlias[col])}`
  );

  const selectFields: Record<string, any> = {};

  if (countJsonFields) {
    selectFields["_count"] = sql`json_build_object${countJsonFields}`;
  }
  if (minJsonFields) {
    selectFields["_min"] = sql`json_build_object${minJsonFields}`;
  }
  if (maxJsonFields) {
    selectFields["_max"] = sql`json_build_object${maxJsonFields}`;
  }
  if (sumJsonFields) {
    selectFields["_sum"] = sql`json_build_object${sumJsonFields}`;
  }
  if (avgJsonFields) {
    selectFields["_avg"] = sql`json_build_object${avgJsonFields}`;
  }

  const joinEntry = Object.entries(
    operationParameters.groupBy[0]?.join ?? {}
  )[0];
  const [joinTable, joinColumn] = joinEntry
    ? joinEntry
    : [undefined, undefined];

  const joinTableAlias: Table | WithSubquery | undefined = joinTable
    ? tableAliasMapper[joinTable] ?? tables[joinTable]
    : undefined;

  const drizzleWhere = parsePrismaWhere({
    tableSchemas: tables,
    tableName: table,
    where: whereAndArray,
  });

  const dbQuery = db
    .with(tableAlias, ...tableAliases)
    .select({
      [operationParameters.groupBy[0].column]:
        tableAlias[operationParameters.groupBy[0].column],
      ...selectFields,
      // @ts-expect-error - We dont know the columns of joinTableAlias
      ...(joinTable ? { [joinColumn]: joinTableAlias[joinColumn] } : {}),
    })
    .from(tableAlias)
    .groupBy(
      tableAlias[operationParameters.groupBy[0].column],
      // @ts-expect-error - We dont know the columns of joinTableAlias
      ...(joinTable ? [joinTableAlias[joinColumn]] : [])
    )
    .where(drizzleWhere)
    .orderBy(() => {
      const column = tableAlias[operationParameters.orderBy.column];
      const direction =
        operationParameters.orderBy.direction === "asc" ? asc : desc;

      switch (operationParameters.orderBy.function) {
        case "count":
          return direction(count(column));
        case "sum":
          return direction(sum(column));
        case "min":
          return direction(min(column));
        case "max":
          return direction(max(column));
        case "avg":
          return direction(avg(column));
        default:
          return direction(count(column)); // Fallback to count
      }
    })
    .limit(operationParameters.limit);

  if (joinTable) {
    const [currentTableKey, relatedTableKey] = findRelationsBetweenTables(
      table,
      joinTable,
      joinTable,
      tables
    );
    dbQuery.leftJoin(
      joinTableAlias,
      // @ts-expect-error - We dont know the columns of joinTableAlias
      eq(tableAlias[currentTableKey], joinTableAlias[relatedTableKey])
    );
  }

  return dbQuery;
}
