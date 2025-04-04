import assert from "assert";
import { type Query } from "~/types/querySchema";
import { parsePrismaWhere } from "~/operations/utils/prismaToDrizzleWhereConditions";
import {
  asc,
  desc,
  eq,
  getTableColumns,
  SQL,
  sql,
  Table,
  WithSubquery,
} from "drizzle-orm";
import { findRelationsBetweenTables } from "~/operations/utils/findRelationsBetweenTables";
import { loadDrizzleSchema } from "~/util/loadDrizzleSchema";
import { getColumnFromTable } from "~/operations/utils/getColumnFromTable";
import { getRelatedTableNameFromPath } from "../utils/drizzleSchemaHelpers";
import {
  buildJsonObjectSelect,
  jsonAggregate,
} from "../utils/jsonBuilderHelpers";

export async function findMany(db: any, query: Query) {
  assert(query.operation === "findMany", "Invalid inconvo operation");
  const {
    table,
    whereAndArray,
    operationParameters,
    jsonColumnSchema,
    computedColumns,
  } = query;
  const { columns, orderBy, limit } = operationParameters;

  const drizzleSchema = await loadDrizzleSchema();

  const selectColsPerTable: Record<string, string[] | null> = {};
  Object.entries(columns).forEach(([tableRelations, value]) => {
    const colName = tableRelations.split(".").at(-1);
    if (colName === undefined) return;
    selectColsPerTable[colName] = value;
  });

  const tableAliasMapper: Record<string, WithSubquery> = {};
  const tableAliases: WithSubquery[] = [];
  const tablesToAlias =
    jsonColumnSchema?.map((jsonCol) => jsonCol.tableName) || [];
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
          ...getTableColumns(drizzleSchema[table]),
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
        .from(drizzleSchema[table])
    );
    tableAliases.push(tableAlias);
    tableAliasMapper[table] = tableAlias;
  }

  // todo: rename
  const needCtes =
    Object.keys(columns).filter((table) => table !== query.table).length > 0;

  function createInitialCte(
    index: number,
    outerIndex: number,
    table: string,
    tableSchema: any,
    currentTableKey: string,
    jsonFields: [string, unknown][],
    groupBy: boolean
  ) {
    const cte = db
      .$with(`cteInitial${table}${index}${outerIndex}${groupBy ? "_" : ""}`)
      .as(
        db
          .select({
            [currentTableKey]: getColumnFromTable({
              columnName: currentTableKey,
              tableName: table,
              drizzleSchema,
              computedColumns: computedColumns,
            }),
            json_data: buildJsonObjectSelect(jsonFields).as("json_data"),
          })
          .from(tableSchema)
      );

    if (groupBy) {
      const groupedCte = db.$with(`cte${table}${index}${outerIndex}`).as(
        db
          .select({
            [currentTableKey]: cte[currentTableKey],
            json_data: jsonAggregate(cte.json_data).as("json_data"),
          })
          .from(cte)
          .groupBy(cte[currentTableKey])
      );
      return [cte, groupedCte];
    }
    return [cte];
  }

  function createSubsequentCte(
    index: number,
    outerIndex: number,
    table: string,
    tableSchema: any,
    currentTableKey: string,
    jsonFields: [string, unknown][],
    previousTable: any,
    previousTableLinks: string[],
    groupBy: boolean
  ) {
    const cte = db
      .$with(`cteSubs${table}${index}${outerIndex}${groupBy ? "_" : ""}`)
      .as(
        db
          .select({
            [currentTableKey]: getColumnFromTable({
              columnName: currentTableKey,
              tableName: table,
              drizzleSchema,
              computedColumns,
            }),
            json_data: buildJsonObjectSelect(jsonFields).as("json_data"),
          })
          .from(tableSchema)
          .leftJoin(
            previousTable,
            eq(
              getColumnFromTable({
                columnName: previousTableLinks[1],
                tableName: table,
                drizzleSchema,
                computedColumns,
              }),
              previousTable[previousTableLinks[0]]
            )
          )
      );
    if (groupBy) {
      const groupedCte = db.$with(`cte${table}${index}${outerIndex}`).as(
        db
          .select({
            [currentTableKey]: cte[currentTableKey],
            json_data: jsonAggregate(cte.json_data).as("json_data"),
          })
          .from(cte)
          .groupBy(cte[currentTableKey])
      );
      return [cte, groupedCte];
    }
    return [cte];
  }

  const tablePaths = Object.keys(operationParameters.columns)
    .filter((table) => table !== query.table)
    .map((table) => table.split("."));
  const dedupedTablePaths = tablePaths.filter(
    (arr, _index, self) =>
      !self.some(
        (other) =>
          other.length > arr.length &&
          other.slice(0, arr.length).every((v, i) => v === arr[i])
      )
  );

  const nestedJsonCtes: WithSubquery[][] = [];
  const outerTableLinks: string[][][] = [];

  if (needCtes) {
    let jsonCtes: WithSubquery[] = [];
    let tableLinks: string[][] = [];
    for (const [outerIndex, tablePath] of dedupedTablePaths.entries()) {
      jsonCtes = [];
      tableLinks = [];
      const reverseTablePath = tablePath.slice().reverse();
      for (const [index, tableRelationName] of reverseTablePath.entries()) {
        if (tableRelationName === query.table) {
          continue;
        }

        const pathToTableName = tablePath.slice(
          0,
          tablePath.indexOf(tableRelationName) + 1
        );
        const tableName = getRelatedTableNameFromPath(
          pathToTableName,
          drizzleSchema
        );

        const pathToRelatedTable = tablePath.slice(
          0,
          tablePath.indexOf(tableRelationName)
        );

        const relatedTableName = getRelatedTableNameFromPath(
          pathToRelatedTable,
          drizzleSchema
        );

        const [relatedTableKey, currentTableKey, groupBy] =
          findRelationsBetweenTables(
            relatedTableName,
            tableName,
            tableRelationName,
            drizzleSchema
          );

        tableLinks.push([currentTableKey, relatedTableKey]);

        const tableSchema =
          tableAliasMapper[tableName] || drizzleSchema[tableName];
        const jsonFields: [string, unknown][] =
          selectColsPerTable[tableRelationName]?.map((col) => {
            const columnParam = getColumnFromTable({
              columnName: col,
              tableName: tableName,
              drizzleSchema,
              computedColumns,
            });
            return [col, columnParam];
          }) ?? [];
        if (index === 0) {
          const ctes = createInitialCte(
            index,
            outerIndex,
            tableName,
            tableSchema,
            currentTableKey,
            jsonFields,
            groupBy
          );
          jsonCtes.push(...ctes);
        } else {
          const previousTableLinks = tableLinks[index - 1];
          const previousTable = jsonCtes[jsonCtes.length - 1];
          const previousTableName = tablePath.toReversed()[index - 1];
          const extendedJsonFields = jsonFields.concat(
            //@ts-expect-error
            [[previousTableName, previousTable["json_data"]]]
          );
          const ctes = createSubsequentCte(
            index,
            outerIndex,
            tableName,
            tableSchema,
            currentTableKey,
            extendedJsonFields,
            previousTable,
            previousTableLinks,
            groupBy
          );
          jsonCtes.push(...ctes);
        }
      }

      nestedJsonCtes.push(jsonCtes);
      outerTableLinks.push(tableLinks);
    }
  }

  const tableSchema: Table | WithSubquery =
    tableAliasMapper[query.table] || drizzleSchema[query.table];

  const rootSelect: { [key: string]: any } = (
    query.operationParameters.columns[table] || []
  ).reduce((acc: { [key: string]: any }, column: string) => {
    acc[column] = getColumnFromTable({
      columnName: column,
      tableName: query.table,
      drizzleSchema,
      computedColumns,
    });
    return acc;
  }, {});

  const dbQuery = db
    .with(...tableAliases, ...nestedJsonCtes.flat())
    .select({
      ...rootSelect,
      ...dedupedTablePaths.reduce(
        (acc: Record<string, any>, tablePath, index) => {
          const tableCte = nestedJsonCtes[index].at(-1);
          const tableName = tablePath[1];
          // @ts-expect-error
          acc[tableName] = sql`${tableCte["json_data"]}`.as(tableName);
          return acc;
        },
        {}
      ),
    })
    .from(tableSchema)
    .where((columns: Record<string, unknown>) =>
      parsePrismaWhere({
        drizzleSchema,
        tableName: table,
        where: whereAndArray,
        columns,
        computedColumns: computedColumns,
      })
    );

  if (needCtes) {
    nestedJsonCtes.forEach((ctes, index) => {
      const tableCte = ctes[ctes.length - 1];
      const finalLink =
        outerTableLinks[index][outerTableLinks[index].length - 1];
      dbQuery.leftJoin(
        tableCte,
        eq(
          getColumnFromTable({
            columnName: finalLink[1],
            tableName: query.table,
            drizzleSchema,
            computedColumns,
          }),
          // @ts-expect-error
          tableCte[finalLink[0]]
        )
      );
    });
  }

  if (orderBy) {
    dbQuery.orderBy((allCols: Record<string, SQL>) => {
      for (const [key, value] of Object.entries(allCols)) {
        if (key === orderBy.column) {
          if (orderBy.direction === "desc") {
            return desc(value);
          } else if (orderBy.direction === "asc") {
            return asc(value);
          }
        }
      }
    });
  }
  if (limit) dbQuery.limit(limit);

  const response = await dbQuery;
  return response;
}
