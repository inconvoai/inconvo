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
import { loadDrizzleSchema } from "~/util/loadDrizzleSchema";
import { getColumnFromTable } from "~/operations/utils/getColumnFromTable";
import { getRelatedTableNameFromPath } from "../utils/drizzleSchemaHelpers";
import {
  buildJsonObjectSelect,
  jsonAggregate,
} from "../utils/jsonBuilderHelpers";
import {
  resolveJoinDescriptor,
  aliasDepth,
} from "../utils/joinDescriptorHelpers";
import { findRelationsBetweenTables } from "~/operations/utils/findRelationsBetweenTables";

export async function findMany(db: any, query: Query) {
  assert(query.operation === "findMany", "Invalid inconvo operation");
  const {
    table,
    whereAndArray,
    operationParameters,
    jsonColumnSchema,
    computedColumns,
  } = query;
  const { select, orderBy, limit } = operationParameters;

  const drizzleSchema = await loadDrizzleSchema();

  const resolvedJoins =
    operationParameters.joins
      ?.map((join) =>
        resolveJoinDescriptor({
          alias: join.name ?? join.table,
          tableName: join.table,
          path: join.path,
        })
      )
      .sort((a, b) => aliasDepth(b.alias) - aliasDepth(a.alias)) ?? [];

  const joinDescriptorMap = new Map(
    resolvedJoins.map((join) => [join.alias, join])
  );

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
    Object.keys(select).filter((tableAlias) => tableAlias !== query.table)
      .length > 0;

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

  const tablePaths = Object.keys(select)
    .filter((alias) => alias !== query.table)
    .map((alias) => alias.split("."));
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
      const aliasKey = tablePath.join(".");
      const joinDescriptor = joinDescriptorMap.get(aliasKey);

      if (!joinDescriptor) {
        throw new Error(`Join descriptor for alias ${aliasKey} not provided.`);
      }

      for (const [index, tableRelationName] of reverseTablePath.entries()) {
        if (tableRelationName === query.table) {
          continue;
        }

        const hopIndex = joinDescriptor.hops.length - 1 - index;
        const hopMetadata =
          joinDescriptor.hops[hopIndex] ?? joinDescriptor.hops[0];

        const targetTableName =
          hopMetadata?.target[0]?.tableName ??
          getRelatedTableNameFromPath(
            tablePath.slice(0, tablePath.indexOf(tableRelationName) + 1),
            drizzleSchema
          );

        const sourceTableName =
          hopMetadata?.source[0]?.tableName ??
          getRelatedTableNameFromPath(
            tablePath.slice(0, tablePath.indexOf(tableRelationName)),
            drizzleSchema
          );

        const currentTableKey = hopMetadata?.target[0]?.columnName;
        const relatedTableKey = hopMetadata?.source[0]?.columnName;

        if (!currentTableKey || !relatedTableKey) {
          throw new Error(
            `Unable to resolve join columns for alias ${aliasKey} (segment ${tableRelationName}).`
          );
        }

        let groupBy: boolean;
        try {
          const [, , groupByFlag = true] = findRelationsBetweenTables(
            sourceTableName,
            targetTableName,
            tableRelationName,
            drizzleSchema
          );
          groupBy = groupByFlag;
        } catch {
          groupBy = true;
        }

        tableLinks.push([currentTableKey, relatedTableKey]);

        const tableSchema =
          tableAliasMapper[targetTableName] || drizzleSchema[targetTableName];
        const aliasForSegment = tablePath
          .slice(0, tablePath.length - index)
          .join(".");
        const selectedColumns = select[aliasForSegment] ?? [];
        const jsonFields: [string, unknown][] = selectedColumns.map((col) => {
          const columnParam = getColumnFromTable({
            columnName: col,
            tableName: targetTableName,
            drizzleSchema,
            computedColumns,
          });
          return [col, columnParam];
        });
        if (index === 0) {
          const ctes = createInitialCte(
            index,
            outerIndex,
            targetTableName,
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
            targetTableName,
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

  const rootSelect: { [key: string]: any } = (select[table] || []).reduce(
    (acc: { [key: string]: any }, column: string) => {
      acc[column] = getColumnFromTable({
        columnName: column,
        tableName: query.table,
        drizzleSchema,
        computedColumns,
      });
      return acc;
    },
    {}
  );

  const dbQuery = db
    .with(...tableAliases, ...nestedJsonCtes.flat())
    .select({
      ...rootSelect,
      ...dedupedTablePaths.reduce(
        (acc: Record<string, any>, tablePath, index) => {
          const tableCte = nestedJsonCtes[index].at(-1);
          if (!tableCte) {
            return acc;
          }
          const tableAlias = tablePath.join(".");
          acc[tableAlias] = sql`${tableCte}.json_data`.as(tableAlias);
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
      if (orderBy.direction === "desc") {
        return desc(
          getColumnFromTable({
            columnName: orderBy.column,
            tableName: query.table,
            drizzleSchema,
            computedColumns,
          })
        );
      }
      return asc(
        getColumnFromTable({
          columnName: orderBy.column,
          tableName: query.table,
          drizzleSchema,
          computedColumns,
        })
      );
    });
  }

  if (limit) dbQuery.limit(limit);

  const response = await dbQuery;
  return { query: dbQuery.toSQL(), data: response };
}
