import type { Schema } from "~/server/db/schema";
import type {
  AggregateQuery,
  CountByTemporalComponentQuery,
  CountQuery,
  CountRelationsQuery,
  CountWithJoinQuery,
  DateCondition,
  FindDistinctByEditDistanceQuery,
  FindDistinctQuery,
  FindManyQuery,
  GroupByDateIntervalQuery,
  GroupByQuery,
  QuestionConditions,
  TableConditions,
} from "~/server/userDatabaseConnector/types";
import type { DBQuery, Operation } from "../types";
import { generateJoinedTables } from "./tableRelations";

export type ColumnLookup = Map<string, Map<string, string>>;
export type ColumnAliasMap = Record<
  string,
  Array<{
    name: string;
    dbName: string;
  }>
>;
type QuestionConditionList = NonNullable<QuestionConditions>["AND"];
type QuestionConditionNode = QuestionConditionList[number];

function recordAlias(
  tracker: ColumnAliasMap | undefined,
  tableName: string,
  original: string,
  canonical: string
) {
  if (!tracker) return;
  if (original === canonical) return;
  const list = tracker[tableName] ?? (tracker[tableName] = []);
  const exists = list.some(
    (entry) => entry.name === original && entry.dbName === canonical
  );
  if (!exists) {
    list.push({ name: original, dbName: canonical });
  }
}

export function buildColumnLookup(schema: Schema): ColumnLookup {
  const lookup: ColumnLookup = new Map();
  schema.forEach((table) => {
    const tableMap = new Map<string, string>();
    table.columns?.forEach((column) => {
      const canonical = column.dbName ?? column.name;
      tableMap.set(column.name, canonical);
      tableMap.set(canonical, canonical);
    });
    table.computedColumns?.forEach((computed) => {
      tableMap.set(computed.name, computed.name);
    });
    lookup.set(table.name, tableMap);
  });
  return lookup;
}

export function normalizeTableConditions(
  tableConditions: TableConditions,
  tableName: string,
  lookup: ColumnLookup,
  aliasTracker?: ColumnAliasMap
): TableConditions {
  if (!tableConditions) return null;
  return tableConditions.map((condition) => ({
    ...condition,
    column: normalizeColumnName(
      lookup,
      tableName,
      condition.column,
      aliasTracker
    ),
  }));
}

export function normalizeQuestionConditions(
  questionConditions: QuestionConditions,
  tableName: string,
  schema: Schema,
  lookup: ColumnLookup,
  aliasTracker?: ColumnAliasMap
): QuestionConditions {
  if (!questionConditions) return null;
  if (!questionConditions.AND) return questionConditions;
  const normalized = questionConditions.AND.map(
    (filter): QuestionConditionNode =>
      normalizeQuestionConditionNode(
        filter,
        tableName,
        schema,
        lookup,
        aliasTracker
      )
  );
  return {
    AND: normalized,
  };
}

function normalizeQuestionConditionNode(
  node: unknown,
  tableName: string,
  schema: Schema,
  lookup: ColumnLookup,
  aliasTracker?: ColumnAliasMap
): QuestionConditionNode {
  if (!node || typeof node !== "object") {
    return node as QuestionConditionNode;
  }

  const tableSchema = getTableSchema(schema, tableName);
  const result: Record<string, unknown> = {};

  Object.entries(node as Record<string, unknown>).forEach(([key, value]) => {
    if (key === "AND" || key === "OR" || key === "NOT") {
      if (Array.isArray(value)) {
        result[key] = value.map((item) =>
          normalizeQuestionConditionNode(
            item,
            tableName,
            schema,
            lookup,
            aliasTracker
          )
        );
      } else {
        result[key] = value;
      }
      return;
    }

    const relation = tableSchema?.outwardRelations?.find(
      (rel) => rel.name === key
    );

    if (relation) {
      if (!value || typeof value !== "object") {
        result[key] = value;
        return;
      }
      const relationValue = value as Record<string, unknown>;
      const normalizedRelationValue: Record<string, unknown> = {};
      Object.entries(relationValue).forEach(([relationKey, relationClause]) => {
        if (relationClause === null) {
          normalizedRelationValue[relationKey] = null;
          return;
        }
        if (Array.isArray(relationClause)) {
          const normalizedArray = relationClause.map((item) =>
            normalizeQuestionConditionNode(
              item,
              relation.targetTable.name,
              schema,
              lookup,
              aliasTracker
            )
          );
          normalizedRelationValue[relationKey] =
            normalizedArray as unknown as QuestionConditionNode;
          return;
        }
        if (typeof relationClause === "object") {
          normalizedRelationValue[relationKey] = normalizeQuestionConditionNode(
            relationClause,
            relation.targetTable.name,
            schema,
            lookup,
            aliasTracker
          );
          return;
        }
        normalizedRelationValue[relationKey] = relationClause;
      });
      result[key] = normalizedRelationValue;
      return;
    }

    const normalizedKey = normalizeColumnName(
      lookup,
      tableName,
      key,
      aliasTracker
    );
    if (value && typeof value === "object" && !Array.isArray(value)) {
      result[normalizedKey] = {
        ...(value as Record<string, unknown>),
      };
    } else {
      result[normalizedKey] = value;
    }
  });

  return result as QuestionConditionNode;
}

export function normalizeDateCondition(
  dateCondition: DateCondition,
  tableName: string,
  lookup: ColumnLookup,
  aliasTracker?: ColumnAliasMap
): DateCondition {
  if (!dateCondition) return null;
  return {
    OR: dateCondition.OR.map((orClause) => ({
      AND: orClause.AND.map((andClause) => ({
        ...andClause,
        column: normalizeColumnName(
          lookup,
          tableName,
          andClause.column,
          aliasTracker
        ),
      })),
    })),
  };
}

export function normalizeQueryColumnReferences(
  query: DBQuery,
  schema: Schema,
  baseTableName: string,
  lookup: ColumnLookup,
  aliasTracker?: ColumnAliasMap
): DBQuery {
  switch (query.operation as Operation) {
    case "findMany": {
      const params =
        query.operationParameters as FindManyQuery["operationParameters"];
      const joinPathMap = buildJoinPathMap(schema, baseTableName);
      const normalizedColumns = Object.entries(params.columns ?? {}).reduce<
        Record<string, string[]>
      >((acc, [path, columns]) => {
        if (!Array.isArray(columns)) return acc;
        acc[path] = columns.map((column) =>
          normalizeColumnForJoinPath(
            lookup,
            joinPathMap,
            baseTableName,
            path,
            column,
            aliasTracker
          )
        );
        return acc;
      }, {});
      const normalizedOrderBy = params.orderBy
        ? {
            ...params.orderBy,
            column: normalizeColumnName(
              lookup,
              baseTableName,
              params.orderBy.column,
              aliasTracker
            ),
          }
        : null;
      return {
        ...query,
        operationParameters: {
          ...params,
          columns: normalizedColumns,
          orderBy: normalizedOrderBy,
        },
      };
    }
    case "findDistinct": {
      const params =
        query.operationParameters as FindDistinctQuery["operationParameters"];
      return {
        ...query,
        operationParameters: {
          ...params,
          column: normalizeColumnName(
            lookup,
            baseTableName,
            params.column,
            aliasTracker
          ),
        },
      };
    }
    case "findDistinctByEditDistance": {
      const params =
        query.operationParameters as FindDistinctByEditDistanceQuery["operationParameters"];
      return {
        ...query,
        operationParameters: {
          ...params,
          column: normalizeColumnName(
            lookup,
            baseTableName,
            params.column,
            aliasTracker
          ),
        },
      };
    }
    case "count": {
      const params =
        query.operationParameters as CountQuery["operationParameters"];
      return {
        ...query,
        operationParameters: {
          ...params,
          count: params.count.map((column) =>
            column === "_all"
              ? column
              : normalizeColumnName(lookup, baseTableName, column, aliasTracker)
          ),
          countDistinct: params.countDistinct
            ? params.countDistinct.map((column) =>
                normalizeColumnName(lookup, baseTableName, column, aliasTracker)
              )
            : null,
        },
      };
    }
    case "countWithJoin": {
      const params =
        query.operationParameters as CountWithJoinQuery["operationParameters"];
      return {
        ...query,
        operationParameters: {
          ...params,
          count: params.count.map((column) =>
            normalizeFullyQualifiedColumn(column, lookup, aliasTracker)
          ),
          countDistinct: params.countDistinct
            ? params.countDistinct.map((column) =>
                normalizeFullyQualifiedColumn(column, lookup, aliasTracker)
              )
            : null,
        },
      };
    }
    case "countRelations": {
      const params =
        query.operationParameters as CountRelationsQuery["operationParameters"];
      const relationMap = buildRelationTargetMap(schema, baseTableName);
      return {
        ...query,
        operationParameters: {
          ...params,
          columns: params.columns.map((column) =>
            normalizeColumnName(lookup, baseTableName, column, aliasTracker)
          ),
          relationsToCount: params.relationsToCount.map((relation) => {
            const targetTable = relationMap[relation.name];
            if (!targetTable || !relation.distinct) {
              return relation;
            }
            return {
              ...relation,
              distinct: normalizeColumnName(
                lookup,
                targetTable,
                relation.distinct,
                aliasTracker
              ),
            };
          }),
        },
      };
    }
    case "aggregate": {
      const params =
        query.operationParameters as AggregateQuery["operationParameters"];
      const normalizeBaseColumns = (columns: string[] | null) =>
        columns
          ? columns.map((column) =>
              normalizeColumnName(lookup, baseTableName, column, aliasTracker)
            )
          : null;
      return {
        ...query,
        operationParameters: {
          ...params,
          avg: normalizeBaseColumns(params.avg),
          sum: normalizeBaseColumns(params.sum),
          min: normalizeBaseColumns(params.min),
          max: normalizeBaseColumns(params.max),
          count: normalizeBaseColumns(params.count),
          median: normalizeBaseColumns(params.median),
        },
      };
    }
    case "groupByDateInterval": {
      const params =
        query.operationParameters as GroupByDateIntervalQuery["operationParameters"];
      const normalizeBaseColumns = (columns: string[] | null) =>
        columns
          ? columns.map((column) =>
              normalizeColumnName(lookup, baseTableName, column, aliasTracker)
            )
          : null;
      const normalizedOrderBy =
        typeof params.orderBy === "string"
          ? params.orderBy
          : {
              ...params.orderBy,
              column: normalizeColumnName(
                lookup,
                baseTableName,
                params.orderBy.column,
                aliasTracker
              ),
            };
      return {
        ...query,
        operationParameters: {
          ...params,
          dateColumn: normalizeColumnName(
            lookup,
            baseTableName,
            params.dateColumn,
            aliasTracker
          ),
          count: normalizeBaseColumns(params.count),
          sum: normalizeBaseColumns(params.sum),
          min: normalizeBaseColumns(params.min),
          max: normalizeBaseColumns(params.max),
          avg: normalizeBaseColumns(params.avg),
          orderBy: normalizedOrderBy,
        },
      };
    }
    case "groupBy": {
      const params =
        query.operationParameters as GroupByQuery["operationParameters"];
      const normalizeAgg = (
        aggregate: { columns: string[] } | null
      ): { columns: string[] } | null => {
        if (!aggregate) return null;
        return {
          columns: aggregate.columns.map((column) =>
            normalizeFullyQualifiedColumn(column, lookup, aliasTracker)
          ),
        };
      };
      return {
        ...query,
        operationParameters: {
          ...params,
          groupBy: params.groupBy.map((column) =>
            normalizeFullyQualifiedColumn(column, lookup, aliasTracker)
          ),
          count: normalizeAgg(params.count),
          sum: normalizeAgg(params.sum),
          min: normalizeAgg(params.min),
          max: normalizeAgg(params.max),
          avg: normalizeAgg(params.avg),
          orderBy: {
            ...params.orderBy,
            column: normalizeFullyQualifiedColumn(
              params.orderBy.column,
              lookup,
              aliasTracker
            ),
          },
        },
      };
    }
    case "countByTemporalComponent": {
      const params =
        query.operationParameters as CountByTemporalComponentQuery["operationParameters"];
      return {
        ...query,
        operationParameters: {
          ...params,
          dateColumn: normalizeColumnName(
            lookup,
            baseTableName,
            params.dateColumn,
            aliasTracker
          ),
        },
      };
    }
    default:
      return query;
  }
}

function normalizeColumnName(
  lookup: ColumnLookup,
  tableName: string,
  columnName: string,
  aliasTracker?: ColumnAliasMap
) {
  const canonical = lookup.get(tableName)?.get(columnName) ?? columnName;
  recordAlias(aliasTracker, tableName, columnName, canonical);
  return canonical;
}

function buildJoinPathMap(schema: Schema, baseTableName: string) {
  const depth = Math.max(schema.length, 1);
  const { iqlPaths } = generateJoinedTables(schema, baseTableName, depth);
  return iqlPaths;
}

function normalizeColumnForJoinPath(
  lookup: ColumnLookup,
  joinPathMap: Record<string, string>,
  baseTable: string,
  path: string,
  column: string,
  aliasTracker?: ColumnAliasMap
) {
  const tableName =
    path === baseTable ? baseTable : joinPathMap[path] ?? undefined;
  if (!tableName) return column;
  return normalizeColumnName(lookup, tableName, column, aliasTracker);
}

function normalizeFullyQualifiedColumn(
  column: string,
  lookup: ColumnLookup,
  aliasTracker?: ColumnAliasMap
) {
  const separatorIndex = column.indexOf(".");
  if (separatorIndex === -1) {
    return column;
  }
  const tableName = column.slice(0, separatorIndex);
  const columnName = column.slice(separatorIndex + 1);
  const normalizedColumn = normalizeColumnName(
    lookup,
    tableName,
    columnName,
    aliasTracker
  );
  return `${tableName}.${normalizedColumn}`;
}

function getTableSchema(schema: Schema, tableName: string) {
  return schema.find((table) => table.name === tableName);
}

function buildRelationTargetMap(schema: Schema, tableName: string) {
  const tableSchema = getTableSchema(schema, tableName);
  if (!tableSchema?.outwardRelations) {
    return {};
  }
  return tableSchema.outwardRelations.reduce<Record<string, string>>(
    (acc, relation) => {
      acc[relation.name] = relation.targetTable.name;
      return acc;
    },
    {}
  );
}
