import { type Query } from "~/types/querySchema";
import { parsePrismaWhere } from "~/operations/utils/prismaToDrizzleWhereConditions";
import { eq, count, sql, desc, asc, countDistinct } from "drizzle-orm";
import { loadDrizzleSchema } from "~/util/loadDrizzleSchema";
import { getRelatedTableNameFromPath } from "~/operations/utils/drizzleSchemaHelpers";
import { findRelationsBetweenTables } from "../utils/findRelationsBetweenTables";
import assert from "assert";

export async function countRelations(db: any, query: Query) {
  assert(query.operation === "countRelations", "Invalid inconvo operation");
  const { table, whereAndArray, operationParameters } = query;

  const tables = await loadDrizzleSchema();

  const relationCountQueries: [any, string, string, string, string, string][] =
    operationParameters.relationsToCount.map((relation) => {
      const targetTableName = getRelatedTableNameFromPath(
        [table, relation.name],
        tables
      );
      const [sourceTableKey, relationTableKey] = findRelationsBetweenTables(
        table,
        targetTableName,
        relation.name,
        tables
      );

      const cte = db.$with(`${relation.name}_count`).as(
        db
          .select({
            [relationTableKey]: tables[targetTableName][relationTableKey],
            [`${relation.name}Count`]: relation.distinct
              ? countDistinct(tables[targetTableName][relation.distinct]).as(
                  `distinct${relation.name}Count`
                )
              : count(tables[targetTableName][relationTableKey]).as(
                  `${relation.name}Count`
                ),
          })
          .from(tables[targetTableName])
          .groupBy(tables[targetTableName][relationTableKey])
      );

      return [
        cte,
        relation.name,
        relation.distinct
          ? `distinct${relation.name}Count`
          : `${relation.name}Count`,
        targetTableName,
        sourceTableKey,
        relationTableKey,
      ];
    });

  const drizzleWhere = parsePrismaWhere({
    tableSchemas: tables,
    tableName: table,
    where: whereAndArray,
  });

  const baseTableColumns: { [key: string]: any } = (
    query.operationParameters.columns || []
  ).reduce((acc: { [key: string]: any }, column: string) => {
    acc[column] = tables[table][column];
    return acc;
  }, {});

  const relationCountColumns: any = relationCountQueries.reduce(
    (acc: any, [cte, relationName, relationColName]: any) => {
      const countColumn = cte[relationColName];
      acc[relationColName] = sql`COALESCE(${countColumn}, 0)`
        .mapWith(Number)
        .as(relationColName);
      return acc;
    },
    {}
  );

  const relationCountCTEs = relationCountQueries.map(
    (relationCountQuery) => relationCountQuery[0]
  );

  const dbQuery = db
    .with(...relationCountCTEs)
    .select({
      ...baseTableColumns,
      ...relationCountColumns,
    })
    .from(tables[table])
    .where(drizzleWhere);

  for (const relationCountMapping of relationCountQueries || []) {
    const [
      cte,
      relationName,
      relationColName,
      targetTableName,
      sourceTableKey,
      relationTableKey,
    ] = relationCountMapping;
    dbQuery.leftJoin(
      cte,
      eq(cte[relationTableKey], tables[table][sourceTableKey])
    );
  }

  if (operationParameters.orderBy) {
    dbQuery.orderBy((allCols: any) => {
      // Redundant check, needed for typescript
      assert(operationParameters.orderBy, "Order by is required");
      const { relation: relationName, direction } = operationParameters.orderBy;
      const countColumn = relationCountQueries.find((relationCountQuery) =>
        relationCountQuery[1].includes(relationName)
      )?.[2];
      assert(countColumn, `Count column ${relationName} not found`);
      return direction === "asc"
        ? asc(allCols[countColumn])
        : desc(allCols[countColumn]);
    });
  }

  if (operationParameters.limit) {
    dbQuery.limit(operationParameters.limit);
  }

  const response = await dbQuery;
  return response.length > 0 ? response : 0;
}
