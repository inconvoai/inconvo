import { type Query } from "~/types/querySchema";
import { parsePrismaWhere } from "~/operations/utils/prismaToDrizzleWhereConditions";
import { asc, and, count, countDistinct, desc, eq, sql } from "drizzle-orm";
import { loadDrizzleSchema } from "~/util/loadDrizzleSchema";
import { getColumnFromTable } from "../utils/getColumnFromTable";
import assert from "assert";
import { normaliseJoinHop, parseQualifiedColumn } from "../utils/joinDescriptorHelpers";

type RelationCountPlan = {
  cte: any;
  relationAlias: string;
  countColumnAlias: string;
  targetTableName: string;
  targetColumns: ReturnType<typeof normaliseJoinHop>["target"];
  sourceColumns: ReturnType<typeof normaliseJoinHop>["source"];
};

export async function countRelations(db: any, query: Query) {
  assert(query.operation === "countRelations", "Invalid inconvo operation");
  const { table, whereAndArray, operationParameters, computedColumns } = query;

  const drizzleSchema = await loadDrizzleSchema();

  const joinDescriptors = new Map(
    (operationParameters.joins ?? []).map((join) => [join.name ?? join.table, join])
  );

  const relationCountPlans: RelationCountPlan[] =
    operationParameters.relationsToCount.map((relation) => {
      const joinDescriptor = joinDescriptors.get(relation.name);
      if (!joinDescriptor) {
        throw new Error(
          `Join descriptor for relation ${relation.name} not provided in joins array.`
        );
      }

      if (joinDescriptor.path.length === 0) {
        throw new Error(
          `Join descriptor for relation ${relation.name} must include at least one hop.`
        );
      }

      if (joinDescriptor.path.length > 1) {
        throw new Error(
          `countRelations currently supports direct relations only. Found ${joinDescriptor.path.length} hops for ${relation.name}.`
        );
      }

      const hopMetadata = normaliseJoinHop(joinDescriptor.path[0]);
      const targetColumns = hopMetadata.target;
      const sourceColumns = hopMetadata.source;

      if (
        !sourceColumns.every((column) => column.tableName === table)
      ) {
        throw new Error(
          `Relation ${relation.name} must originate from base table ${table}.`
        );
      }

      const targetTableName = targetColumns[0]?.tableName;
      if (!targetTableName) {
        throw new Error(
          `Unable to determine target table for relation ${relation.name}.`
        );
      }

      const cteAlias = `${relation.name.replace(/\W+/g, "_")}_count`;
      const countColumnAlias = relation.distinct
        ? `${relation.name.replace(/\W+/g, "_")}_distinctCount`
        : `${relation.name.replace(/\W+/g, "_")}_count`;

      const distinctColumnQualified = relation.distinct
        ? parseQualifiedColumn(
            relation.distinct.includes(".")
              ? relation.distinct
              : `${targetTableName}.${relation.distinct}`
          )
        : null;

      if (
        distinctColumnQualified &&
        distinctColumnQualified.tableName !== targetTableName
      ) {
        throw new Error(
          `Distinct column ${relation.distinct} must belong to relation table ${targetTableName}.`
        );
      }

      const cteSelect: Record<string, any> = {};
      targetColumns.forEach((column) => {
        cteSelect[column.columnName] =
          drizzleSchema[targetTableName][column.columnName];
      });

      const countExpression = distinctColumnQualified
        ? countDistinct(
            drizzleSchema[targetTableName][distinctColumnQualified.columnName]
          ).as(countColumnAlias)
        : count(
            drizzleSchema[targetTableName][targetColumns[0].columnName]
          ).as(countColumnAlias);

      const cte = db.$with(cteAlias).as(
        db
          .select({
            ...cteSelect,
            [countColumnAlias]: countExpression,
          })
          .from(drizzleSchema[targetTableName])
          .groupBy(
            ...targetColumns.map(
              (column) => drizzleSchema[targetTableName][column.columnName]
            )
          )
      );

      return {
        cte,
        relationAlias: relation.name,
        countColumnAlias,
        targetTableName,
        targetColumns,
        sourceColumns,
      };
    });

  const baseTableColumns: Record<string, any> = (
    operationParameters.columns || []
  ).reduce((acc: Record<string, any>, column: string) => {
    acc[column] = sql`${getColumnFromTable({
      columnName: column,
      tableName: table,
      drizzleSchema,
      computedColumns,
    })}`.as(column);
    return acc;
  }, {});

  const relationCountColumns: Record<string, any> = relationCountPlans.reduce(
    (acc, plan) => {
      const countColumn = plan.cte[plan.countColumnAlias];
      acc[plan.countColumnAlias] = sql`COALESCE(${countColumn}, 0)`
        .mapWith(Number)
        .as(plan.countColumnAlias);
      return acc;
    },
    {} as Record<string, any>
  );

  const relationCountCTEs = relationCountPlans.map((plan) => plan.cte);

  let dbQuery = db
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
        computedColumns,
      })
    );

  for (const plan of relationCountPlans) {
    let joinCondition: ReturnType<typeof eq> | undefined;
    plan.targetColumns.forEach((targetColumn, index) => {
      const sourceColumn = plan.sourceColumns[index];
      const condition = eq(
        plan.cte[targetColumn.columnName],
        drizzleSchema[sourceColumn.tableName][sourceColumn.columnName]
      );
      joinCondition = joinCondition ? and(joinCondition, condition) : condition;
    });

    if (!joinCondition) {
      throw new Error(
        `Join descriptor for relation ${plan.relationAlias} produced no join conditions.`
      );
    }

    dbQuery = dbQuery.leftJoin(plan.cte, joinCondition);
  }

  if (operationParameters.orderBy) {
    dbQuery.orderBy((allCols: any) => {
      const { name: relationName, direction } = operationParameters.orderBy!;
      const countColumn = relationCountPlans.find(
        (plan) => plan.relationAlias === relationName
      )?.countColumnAlias;
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
  return { query: dbQuery.toSQL(), data: response.length > 0 ? response : 0 };
}
