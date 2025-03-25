import { type Query } from "~/types/querySchema";
import { parsePrismaWhere } from "~/operations/utils/prismaToDrizzleWhereConditions";
import { asc, desc, eq, getTableColumns, sql, Table } from "drizzle-orm";
import { getTableConfig } from "drizzle-orm/pg-core";
import { findRelationsBetweenTables } from "~/operations/utils/findRelationsBetweenTables";
import { loadDrizzleSchema } from "~/util/loadDrizzleSchema";
import assert from "assert";

export async function countRelations(db: any, query: Query) {
  assert(query.operation === "countRelations", "Invalid inconvo operation");
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

  // FIXME: there is an issue here if you try to filter on the relations
  // i.e https://www.prisma.io/docs/orm/prisma-client/queries/relation-queries#filter-on-absence-of--to-many-records
  const drizzleWhere = parsePrismaWhere({
    tableSchemas: tables,
    tableName: table,
    where: whereAndArray,
  });

  function getTablePrimaryKey(table: Table) {
    const { columns } = getTableConfig(table);
    for (const column of columns) {
      if (column.primary) {
        return column.name;
      }
    }
    throw new Error("Table does not have a primary key");
  }

  const relationsToCount =
    operationParameters.relationsToCount?.map((table) => {
      const primaryKey = getTablePrimaryKey(tables[table.name]);
      // Use the distinct column if specified, otherwise use the primary key
      const distinctColumn = table.distinct
        ? tables[table.name][table.distinct]
        : tables[table.name][primaryKey];

      return sql`${table.name}::text,  COUNT(DISTINCT ${distinctColumn})::numeric`;
    }) || [];

  const rootSelect: { [key: string]: any } = (
    query.operationParameters.columns || []
  ).reduce((acc: { [key: string]: any }, column: string) => {
    acc[column] = tableAlias[column];
    return acc;
  }, {});

  const dbQuery = db
    .with(tableAlias)
    .select({
      ...rootSelect,
      _count: sql<number>`JSON_BUILD_OBJECT${relationsToCount}`.as("_count"),
    })
    .from(tableAlias)
    .where(drizzleWhere);

  for (const joinTable of operationParameters.relationsToCount) {
    const [currentTableKey, relatedTableKey] = findRelationsBetweenTables(
      tables[table],
      tables[joinTable.name]
    );
    dbQuery.leftJoin(
      tables[joinTable.name],
      eq(tables[joinTable.name][relatedTableKey], tableAlias[currentTableKey])
    );
  }

  const groupByColumns = operationParameters.columns.map(
    (col) => tableAlias[col]
  );

  dbQuery.groupBy(...groupByColumns);

  if (operationParameters.orderBy) {
    const relationName = operationParameters.orderBy.relation;

    // Create a direct SQL expression to get the count for the specific relation
    const matchingRelation = operationParameters.relationsToCount?.find(
      (rel) => rel.name === relationName
    );

    if (matchingRelation) {
      const primaryKey = getTablePrimaryKey(tables[relationName]);
      // Use the distinct column if specified, otherwise use the primary key
      const distinctColumn = matchingRelation.distinct
        ? tables[relationName][matchingRelation.distinct]
        : tables[relationName][primaryKey];

      const countExpression = sql`COUNT(DISTINCT ${distinctColumn})`;

      if (operationParameters.orderBy.direction === "desc") {
        dbQuery.orderBy(desc(countExpression));
      } else {
        dbQuery.orderBy(asc(countExpression));
      }
    }
  }
  if (operationParameters.limit) {
    dbQuery.limit(operationParameters.limit);
  }

  const response = await dbQuery;
  return response.length > 0 ? response : 0;
}
