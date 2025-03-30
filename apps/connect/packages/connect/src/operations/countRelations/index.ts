import { type Query } from "~/types/querySchema";
import { parsePrismaWhere } from "~/operations/utils/prismaToDrizzleWhereConditions";
import { asc, desc, eq, Relation, sql } from "drizzle-orm";
import { loadDrizzleSchema } from "~/util/loadDrizzleSchema";
import assert from "assert";
import { SchemaResponse } from "~/types/types";
import { buildSchema } from "~/util/buildSchema";
import {
  getAUniqueKeyInTable,
  getRelationsForTable,
} from "~/operations/utils/drizzleSchemaHelpers";

function getJoinTargetTableName(
  startTableName: string,
  relationName: string,
  schema: SchemaResponse
) {
  const tableSchema = schema.tables.find(
    (table) => table.name === startTableName
  );
  assert(tableSchema, `Table ${startTableName} not found in schema`);
  const relation = tableSchema?.relations?.find(
    (relation) => relation.name === relationName
  );
  assert(
    relation,
    `Relation ${relationName} not found in table ${startTableName}`
  );
  return relation.targetTable;
}

function getDistinctColumn(
  tables: Record<string, any>,
  tableName: string,
  distinctColumn: string | null
) {
  if (distinctColumn) {
    return tables[tableName][distinctColumn];
  }
  const uniqueKey = getAUniqueKeyInTable(tables[tableName]);
  return tables[tableName][uniqueKey];
}

function findKeysFromRelation(
  sourceTable: string,
  relationName: string,
  drizzleTables: Record<string, any>
) {
  const sourceTableRelations = getRelationsForTable(sourceTable, drizzleTables);
  let relationOfInterest: Relation | null = null;
  for (const [relationKey, relationValue] of Object.entries(
    sourceTableRelations
  )) {
    if (relationValue.fieldName === relationName) {
      relationOfInterest = relationValue;
      break;
    }
  }
  if (!relationOfInterest) {
    throw new Error(
      `Relation ${relationName} not found in table ${sourceTable}`
    );
  }
  const targetTableName = relationOfInterest.referencedTableName;
  const dbRelationName = relationOfInterest.relationName;
  const targetTableRelations = getRelationsForTable(
    targetTableName,
    drizzleTables
  );
  for (const [relationKey, relationValue] of Object.entries(
    targetTableRelations
  )) {
    if (dbRelationName && relationValue.relationName === dbRelationName) {
      return {
        relationTableKey: relationValue.config.fields[0].name,
        sourceTableKey: relationValue.config.references[0].name,
        targetTableName: targetTableName,
      };
    } else if (relationValue.fieldName === sourceTable) {
      return {
        relationTableKey: relationValue.config.fields[0].name,
        sourceTableKey: relationValue.config.references[0].name,
        targetTableName: targetTableName,
      };
    }
  }
  throw new Error(
    `Relation ${relationName} not found in table ${targetTableName}`
  );
}

export async function countRelations(db: any, query: Query) {
  assert(query.operation === "countRelations", "Invalid inconvo operation");
  const { table, whereAndArray, operationParameters } = query;

  const tables = await loadDrizzleSchema();
  const schema = await buildSchema();

  const relationNameToJoinTableMap: Record<string, string> = {};
  for (const { name } of operationParameters.relationsToCount) {
    const joinTableName = getJoinTargetTableName(table, name, schema);
    relationNameToJoinTableMap[name] = joinTableName;
  }

  const drizzleWhere = parsePrismaWhere({
    tableSchemas: tables,
    tableName: table,
    where: whereAndArray,
  });

  const relationsToCount =
    operationParameters.relationsToCount?.map((relation) => {
      const distinctColumn = getDistinctColumn(
        tables,
        relationNameToJoinTableMap[relation.name],
        relation.distinct
      );
      return sql`${relation.name}::text, COUNT(DISTINCT ${distinctColumn})::numeric`;
    }) || [];

  const rootSelect: { [key: string]: any } = (
    query.operationParameters.columns || []
  ).reduce((acc: { [key: string]: any }, column: string) => {
    acc[column] = tables[table][column];
    return acc;
  }, {});

  const dbQuery = db
    .select({
      ...rootSelect,
      _count: sql<number>`JSON_BUILD_OBJECT${relationsToCount}`.as("_count"),
    })
    .from(tables[table])
    .where(drizzleWhere);

  for (const relation of operationParameters.relationsToCount || []) {
    const { targetTableName, sourceTableKey, relationTableKey } =
      findKeysFromRelation(table, relation.name, tables);
    dbQuery.leftJoin(
      tables[targetTableName],
      eq(
        tables[targetTableName][relationTableKey],
        tables[table][sourceTableKey]
      )
    );
  }

  const groupByColumns = (operationParameters.columns || []).map(
    (col: string) => tables[table][col]
  );

  dbQuery.groupBy(...groupByColumns);

  if (operationParameters.orderBy) {
    const { relation: relationName, direction } = operationParameters.orderBy;
    const matchingRelation = operationParameters.relationsToCount?.find(
      (rel: any) => rel.name === relationName
    );

    if (matchingRelation) {
      const distinctColumn = getDistinctColumn(
        tables,
        relationNameToJoinTableMap[relationName],
        matchingRelation.distinct
      );
      const countExpression = sql`COUNT(DISTINCT ${distinctColumn})`;
      dbQuery.orderBy(
        direction === "desc" ? desc(countExpression) : asc(countExpression)
      );
    }
  }

  if (operationParameters.limit) {
    dbQuery.limit(operationParameters.limit);
  }

  const response = await dbQuery;
  return response.length > 0 ? response : 0;
}
