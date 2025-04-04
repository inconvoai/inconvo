import { type Query } from "~/types/querySchema";
import { parsePrismaWhere } from "~/operations/utils/prismaToDrizzleWhereConditions";
import { eq, count, sql, desc, asc, countDistinct } from "drizzle-orm";
import { loadDrizzleSchema } from "~/util/loadDrizzleSchema";
import { getRelatedTableNameFromPath } from "~/operations/utils/drizzleSchemaHelpers";
import { findRelationsBetweenTables } from "../utils/findRelationsBetweenTables";
import assert from "assert";

export async function countRelations(db: any, query: Query) {
  assert(query.operation === "countRelations", "Invalid inconvo operation");
  const { table, whereAndArray, operationParameters, computedColumns } = query;

  const drizzleSchema = await loadDrizzleSchema();

  const relationCountQueries: [any, string, string, string, string, string][] =
    operationParameters.relationsToCount.map((relation) => {
      const targetTableName = getRelatedTableNameFromPath(
        [table, relation.name],
        drizzleSchema
      );
      const [sourceTableKey, relationTableKey] = findRelationsBetweenTables(
        table,
        targetTableName,
        relation.name,
        drizzleSchema
      );

      const countColumnName = relation.distinct
        ? `distinct${relation.name}Count`
        : `${relation.name}Count`;

      const cte = db.$with(`${relation.name}_count`).as(
        db
          .select({
            [relationTableKey]:
              drizzleSchema[targetTableName][relationTableKey],
            [countColumnName]: relation.distinct
              ? countDistinct(
                  drizzleSchema[targetTableName][relation.distinct]
                ).as(countColumnName)
              : count(drizzleSchema[targetTableName][relationTableKey]).as(
                  countColumnName
                ),
          })
          .from(drizzleSchema[targetTableName])
          .groupBy(drizzleSchema[targetTableName][relationTableKey])
      );

      return [
        cte,
        relation.name,
        countColumnName,
        targetTableName,
        sourceTableKey,
        relationTableKey,
      ];
    });

  const baseTableColumns: { [key: string]: any } = (
    query.operationParameters.columns || []
  ).reduce((acc: { [key: string]: any }, column: string) => {
    acc[column] = drizzleSchema[table][column];
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
    .from(drizzleSchema[table])
    .where((columns: Record<string, unknown>) =>
      parsePrismaWhere({
        drizzleSchema,
        tableName: table,
        where: whereAndArray,
        columns,
        computedColumns: computedColumns,
      })
    );

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
      eq(cte[relationTableKey], drizzleSchema[table][sourceTableKey])
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
