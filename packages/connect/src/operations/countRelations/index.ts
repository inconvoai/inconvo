import { Kysely, sql } from "kysely";
import type { Query } from "../../types/querySchema";
import { buildWhereConditions } from "../utils/whereConditionBuilder";
import { getAugmentedSchema } from "../../util/augmentedSchemaCache";
import { getColumnFromTable } from "../utils/computedColumns";
import { applyLimit } from "../utils/queryHelpers";
import assert from "assert";
import {
  normaliseJoinHop,
  parseQualifiedColumn,
} from "../utils/joinDescriptorHelpers";

type RelationCountPlan = {
  cteName: string;
  relationAlias: string;
  countColumnAlias: string;
  targetTableName: string;
  targetColumns: ReturnType<typeof normaliseJoinHop>["target"];
  sourceColumns: ReturnType<typeof normaliseJoinHop>["source"];
  cteQuery: any;
};

export async function countRelations(db: Kysely<any>, query: Query) {
  assert(query.operation === "countRelations", "Invalid operation");
  const { table, whereAndArray, operationParameters } = query;
  const { columns, relationsToCount, orderBy, limit } = operationParameters;

  const schema = await getAugmentedSchema();

  const joinDescriptors = new Map(
    (operationParameters.joins ?? []).map((join) => [
      join.name ?? join.table,
      join,
    ]),
  );

  const plans: RelationCountPlan[] = relationsToCount.map((relation) => {
    const joinDescriptor = joinDescriptors.get(relation.name);
    if (!joinDescriptor) {
      throw new Error(
        `Join descriptor for relation ${relation.name} not provided in joins array.`,
      );
    }
    if (joinDescriptor.path.length === 0) {
      throw new Error(
        `Join descriptor for relation ${relation.name} must include at least one hop.`,
      );
    }
    if (joinDescriptor.path.length > 1) {
      throw new Error(
        `countRelations currently supports direct relations only. Found ${joinDescriptor.path.length} hops for ${relation.name}.`,
      );
    }

    const hopMetadata = normaliseJoinHop(joinDescriptor.path[0]!);
    const sourceColumns = hopMetadata.source;
    const targetColumns = hopMetadata.target;

    if (!sourceColumns.every((column) => column.tableName === table)) {
      throw new Error(
        `Relation ${relation.name} must originate from base table ${table}.`,
      );
    }

    const targetTableName = targetColumns[0]?.tableName;
    if (!targetTableName) {
      throw new Error(
        `Unable to determine target table for relation ${relation.name}.`,
      );
    }

    const cteName = `${relation.name.replace(/\W+/g, "_")}_count`;
    const countColumnAlias = relation.distinct
      ? `${relation.name.replace(/\W+/g, "_")}_distinctCount`
      : `${relation.name.replace(/\W+/g, "_")}_count`;

    const distinctColumnQualified = relation.distinct
      ? parseQualifiedColumn(
          relation.distinct.includes(".")
            ? relation.distinct
            : `${targetTableName}.${relation.distinct}`,
        )
      : null;

    if (
      distinctColumnQualified &&
      distinctColumnQualified.tableName !== targetTableName
    ) {
      throw new Error(
        `Distinct column ${relation.distinct} must belong to relation table ${targetTableName}.`,
      );
    }

    let cte = db.selectFrom(targetTableName);

    for (const column of targetColumns) {
      cte = cte.select(
        sql`${sql.ref(`${targetTableName}.${column.columnName}`)}`.as(
          column.columnName,
        ),
      );
    }

    const countExpression = distinctColumnQualified
      ? sql`COUNT(DISTINCT ${sql.ref(
          `${distinctColumnQualified.tableName}.${distinctColumnQualified.columnName}`,
        )})`
      : sql`COUNT(${sql.ref(
          `${targetTableName}.${targetColumns[0]!.columnName}`,
        )})`;

    cte = cte
      .select(countExpression.as(countColumnAlias))
      .groupBy(
        targetColumns.map((column) =>
          sql.ref(`${targetTableName}.${column.columnName}`),
        ) as any,
      );

    return {
      cteName,
      relationAlias: relation.name,
      countColumnAlias,
      targetTableName,
      targetColumns,
      sourceColumns,
      cteQuery: cte,
    };
  });

  let builder: any = db;
  for (const plan of plans) {
    builder = builder.with(plan.cteName, () => plan.cteQuery);
  }

  let selectBuilder = builder.selectFrom(table);

  const baseSelections: any[] = [];
  for (const column of columns ?? []) {
    const columnRef = getColumnFromTable({
      columnName: column,
      tableName: table,
      schema,
    });
    baseSelections.push(columnRef.as(column));
  }
  if (baseSelections.length > 0) {
    selectBuilder = selectBuilder.select(baseSelections);
  }

  for (const plan of plans) {
    selectBuilder = selectBuilder.select(
      sql`COALESCE(${sql.ref(`${plan.cteName}.${plan.countColumnAlias}`)}, 0)`.as(
        plan.countColumnAlias,
      ),
    );
  }

  const whereExpr = buildWhereConditions(whereAndArray, table, schema);
  if (whereExpr) {
    selectBuilder = selectBuilder.where(whereExpr);
  }

  for (const plan of plans) {
    selectBuilder = selectBuilder.leftJoin(plan.cteName, (join: any) => {
      let joinBuilder = join.onRef(
        `${table}.${plan.sourceColumns[0]!.columnName}`,
        "=",
        `${plan.cteName}.${plan.targetColumns[0]!.columnName}`,
      );
      for (let index = 1; index < plan.sourceColumns.length; index++) {
        joinBuilder = joinBuilder.onRef(
          `${table}.${plan.sourceColumns[index]!.columnName}`,
          "=",
          `${plan.cteName}.${plan.targetColumns[index]!.columnName}`,
        );
      }
      return joinBuilder;
    });
  }

  if (orderBy) {
    const plan = plans.find((p) => p.relationAlias === orderBy.name);
    if (!plan) {
      throw new Error(`OrderBy relation ${orderBy.name} not found`);
    }
    selectBuilder = selectBuilder.orderBy(
      sql.ref(plan.countColumnAlias),
      orderBy.direction,
    );
  }

  selectBuilder = applyLimit(selectBuilder, limit);

  const compiled = selectBuilder.compile();
  const rows = await selectBuilder.execute();

  const normalized = rows.map((row: Record<string, unknown>) => {
    const entry: Record<string, unknown> = {};

    for (const [key, value] of Object.entries(row)) {
      const matchingPlan = plans.find((plan) => plan.countColumnAlias === key);

      if (matchingPlan) {
        entry[key] = Number(value ?? 0);
      } else {
        entry[key] = value;
      }
    }

    return entry;
  });

  return {
    query: { sql: compiled.sql, params: compiled.parameters },
    data: normalized.length > 0 ? normalized : 0,
  };
}
