import type { Schema } from "~/server/db/schema";
import type {
  AggregateQuery,
  CountQuery,
  CountRelationsQuery,
  CountWithJoinQuery,
  DateCondition,
  FindDistinctByEditDistanceQuery,
  FindDistinctQuery,
  FindManyQuery,
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

export function canonicalizeTableConditions(
  tableConditions: TableConditions,
  tableName: string,
  lookup: ColumnLookup,
  aliasTracker?: ColumnAliasMap
): TableConditions {
  if (!tableConditions) return null;
  return tableConditions.map((condition) => ({
    ...condition,
    column: canonicalizeColumnName(
      lookup,
      tableName,
      condition.column,
      aliasTracker
    ),
  }));
}

export function canonicalizeQuestionConditions(
  questionConditions: QuestionConditions,
  tableName: string,
  schema: Schema,
  lookup: ColumnLookup,
  aliasTracker?: ColumnAliasMap
): QuestionConditions {
  if (!questionConditions) return null;
  if (!questionConditions.AND) return questionConditions;
  const canonicalized = questionConditions.AND.map(
    (filter): QuestionConditionNode =>
      canonicalizeQuestionConditionNode(
        filter,
        tableName,
        schema,
        lookup,
        aliasTracker
      )
  );
  return {
    AND: canonicalized,
  };
}

function canonicalizeQuestionConditionNode(
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
          canonicalizeQuestionConditionNode(
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
      const canonicalizedRelationValue: Record<string, unknown> = {};
      Object.entries(relationValue).forEach(([relationKey, relationClause]) => {
        if (relationClause === null) {
          canonicalizedRelationValue[relationKey] = null;
          return;
        }
        if (Array.isArray(relationClause)) {
          const canonicalizedArray = relationClause.map((item) =>
            canonicalizeQuestionConditionNode(
              item,
              relation.targetTable.name,
              schema,
              lookup,
              aliasTracker
            )
          );
          canonicalizedRelationValue[relationKey] =
            canonicalizedArray as unknown as QuestionConditionNode;
          return;
        }
        if (typeof relationClause === "object") {
          canonicalizedRelationValue[relationKey] = canonicalizeQuestionConditionNode(
            relationClause,
            relation.targetTable.name,
            schema,
            lookup,
            aliasTracker
          );
          return;
        }
        canonicalizedRelationValue[relationKey] = relationClause;
      });
      result[key] = canonicalizedRelationValue;
      return;
    }

    const canonicalizedKey = canonicalizeColumnName(
      lookup,
      tableName,
      key,
      aliasTracker
    );
    if (value && typeof value === "object" && !Array.isArray(value)) {
      result[canonicalizedKey] = {
        ...(value as Record<string, unknown>),
      };
    } else {
      result[canonicalizedKey] = value;
    }
  });

  return result as QuestionConditionNode;
}

export function canonicalizeDateCondition(
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
        column: canonicalizeColumnName(
          lookup,
          tableName,
          andClause.column,
          aliasTracker
        ),
      })),
    })),
  };
}

export function canonicalizeQueryColumnReferences(
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
      const canonicalizedColumns = Object.entries(params.columns ?? {}).reduce<
        Record<string, string[]>
      >((acc, [path, columns]) => {
        if (!Array.isArray(columns)) return acc;
        acc[path] = columns.map((column) =>
          canonicalizeColumnForJoinPath(
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
      const canonicalizedOrderBy = params.orderBy
        ? {
            ...params.orderBy,
            column: canonicalizeColumnName(
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
          columns: canonicalizedColumns,
          orderBy: canonicalizedOrderBy,
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
          column: canonicalizeColumnName(
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
          column: canonicalizeColumnName(
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
              : canonicalizeColumnName(lookup, baseTableName, column, aliasTracker)
          ),
          countDistinct: params.countDistinct
            ? params.countDistinct.map((column) =>
                canonicalizeColumnName(lookup, baseTableName, column, aliasTracker)
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
            canonicalizeFullyQualifiedColumn(column, lookup, aliasTracker)
          ),
          countDistinct: params.countDistinct
            ? params.countDistinct.map((column) =>
                canonicalizeFullyQualifiedColumn(column, lookup, aliasTracker)
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
            canonicalizeColumnName(lookup, baseTableName, column, aliasTracker)
          ),
          relationsToCount: params.relationsToCount.map((relation) => {
            const targetTable = relationMap[relation.name];
            if (!targetTable || !relation.distinct) {
              return relation;
            }
            return {
              ...relation,
              distinct: canonicalizeColumnName(
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
      const canonicalizeBaseColumns = (columns: string[] | null) =>
        columns
          ? columns.map((column) =>
              canonicalizeColumnName(lookup, baseTableName, column, aliasTracker)
            )
          : null;
      return {
        ...query,
        operationParameters: {
          ...params,
          avg: canonicalizeBaseColumns(params.avg),
          sum: canonicalizeBaseColumns(params.sum),
          min: canonicalizeBaseColumns(params.min),
          max: canonicalizeBaseColumns(params.max),
          count: canonicalizeBaseColumns(params.count),
          median: canonicalizeBaseColumns(params.median),
        },
      };
    }
    case "groupBy": {
      const params =
        query.operationParameters as GroupByQuery["operationParameters"];
      const canonicalizeAgg = (aggregate: string[] | null): string[] | null => {
        if (!aggregate) return null;
        return aggregate.map((column) =>
          canonicalizeFullyQualifiedColumn(column, lookup, aliasTracker)
        );
      };

      const canonicalizedGroupBy = params.groupBy.map((groupKey) => {
        if (groupKey.type === "column") {
          const canonicalizedColumn = canonicalizeFullyQualifiedColumn(
            groupKey.column,
            lookup,
            aliasTracker
          );
          const defaultAlias = groupKey.column;
          const alias =
            (groupKey.alias ?? defaultAlias) === defaultAlias
              ? canonicalizedColumn
              : groupKey.alias ?? canonicalizedColumn;
          return {
            ...groupKey,
            column: canonicalizedColumn,
            alias,
          };
        }

        if (groupKey.type === "dateInterval") {
          const canonicalizedColumn = canonicalizeFullyQualifiedColumn(
            groupKey.column,
            lookup,
            aliasTracker
          );
          const defaultAlias = `${groupKey.column}|${groupKey.interval}`;
          const canonicalizedDefaultAlias = `${canonicalizedColumn}|${groupKey.interval}`;
          const alias =
            (groupKey.alias ?? defaultAlias) === defaultAlias
              ? canonicalizedDefaultAlias
              : groupKey.alias ?? canonicalizedDefaultAlias;
          return {
            ...groupKey,
            column: canonicalizedColumn,
            alias,
          };
        }

        if (groupKey.type === "dateComponent") {
          const canonicalizedColumn = canonicalizeFullyQualifiedColumn(
            groupKey.column,
            lookup,
            aliasTracker
          );
          const defaultAlias = `${groupKey.column}|${groupKey.component}`;
          const canonicalizedDefaultAlias = `${canonicalizedColumn}|${groupKey.component}`;
          const alias =
            (groupKey.alias ?? defaultAlias) === defaultAlias
              ? canonicalizedDefaultAlias
              : groupKey.alias ?? canonicalizedDefaultAlias;
          return {
            ...groupKey,
            column: canonicalizedColumn,
            alias,
          };
        }

        return groupKey;
      });

      const canonicalizedOrderBy =
        params.orderBy.type === "groupKey"
          ? params.orderBy
          : {
              ...params.orderBy,
              column: canonicalizeFullyQualifiedColumn(
                params.orderBy.column,
                lookup,
                aliasTracker
              ),
            };

      return {
        ...query,
        operationParameters: {
          ...params,
          groupBy: canonicalizedGroupBy,
          count: canonicalizeAgg(params.count),
          sum: canonicalizeAgg(params.sum),
          min: canonicalizeAgg(params.min),
          max: canonicalizeAgg(params.max),
          avg: canonicalizeAgg(params.avg),
          orderBy: canonicalizedOrderBy,
        },
      };
    }
    default:
      return query;
  }
}

function canonicalizeColumnName(
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

function canonicalizeColumnForJoinPath(
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
  return canonicalizeColumnName(lookup, tableName, column, aliasTracker);
}

function canonicalizeFullyQualifiedColumn(
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
  const canonicalizedColumn = canonicalizeColumnName(
    lookup,
    tableName,
    columnName,
    aliasTracker
  );
  return `${tableName}.${canonicalizedColumn}`;
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
