import type { Schema } from "@repo/types";
import type {
  AggregateQuery,
  AggregateGroupsQuery,
  CountQuery,
  CountRelationsQuery,
  DateCondition,
  FindDistinctByEditDistanceQuery,
  FindDistinctQuery,
  FindManyQuery,
  GroupByQuery,
  QuestionConditions,
  TableConditions,
  JoinPathHop,
} from "@repo/types";
import type { DBQuery, Operation } from "../types";

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
  canonical: string,
) {
  if (!tracker) return;
  if (original === canonical) return;
  const list = tracker[tableName] ?? (tracker[tableName] = []);
  const exists = list.some(
    (entry) => entry.name === original && entry.dbName === canonical,
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
  aliasTracker?: ColumnAliasMap,
): TableConditions {
  if (!tableConditions) return null;
  return tableConditions.map((condition) => ({
    ...condition,
    column: canonicalizeColumnName(
      lookup,
      tableName,
      condition.column,
      aliasTracker,
    ),
  }));
}

export function canonicalizeQuestionConditions(
  questionConditions: QuestionConditions,
  tableName: string,
  schema: Schema,
  lookup: ColumnLookup,
  aliasTracker?: ColumnAliasMap,
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
        aliasTracker,
      ),
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
  aliasTracker?: ColumnAliasMap,
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
            aliasTracker,
          ),
        );
      } else {
        result[key] = value;
      }
      return;
    }

    const relation = tableSchema?.outwardRelations?.find(
      (rel) => rel.name === key,
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
              aliasTracker,
            ),
          );
          canonicalizedRelationValue[relationKey] =
            canonicalizedArray as unknown as QuestionConditionNode;
          return;
        }
        if (typeof relationClause === "object") {
          canonicalizedRelationValue[relationKey] =
            canonicalizeQuestionConditionNode(
              relationClause,
              relation.targetTable.name,
              schema,
              lookup,
              aliasTracker,
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
      aliasTracker,
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
  aliasTracker?: ColumnAliasMap,
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
          aliasTracker,
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
  aliasTracker?: ColumnAliasMap,
): DBQuery {
  switch (query.operation as Operation) {
    case "findMany": {
      const params =
        query.operationParameters as FindManyQuery["operationParameters"];
      const joinPathMap = buildJoinPathMap(
        baseTableName,
        params.joins ?? undefined,
      );
      const canonicalizedSelect = Object.entries(params.select ?? {}).reduce<
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
            aliasTracker,
          ),
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
              aliasTracker,
            ),
          }
        : null;
      return {
        ...query,
        operationParameters: {
          ...params,
          select: canonicalizedSelect,
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
            aliasTracker,
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
            aliasTracker,
          ),
        },
      };
    }
    case "count": {
      const params =
        query.operationParameters as CountQuery["operationParameters"];
      const canonicalizedJoins = canonicalizeJoinArray(
        params.joins,
        lookup,
        aliasTracker,
      );
      const canonicalizedCount = Array.isArray(params.count)
        ? params.count.map((column) =>
            canonicalizeMetricColumn(
              column,
              baseTableName,
              lookup,
              aliasTracker,
            ),
          )
        : null;
      const canonicalizedCountDistinct = Array.isArray(params.countDistinct)
        ? params.countDistinct.map((column) =>
            canonicalizeMetricColumn(
              column,
              baseTableName,
              lookup,
              aliasTracker,
            ),
          )
        : null;
      return {
        ...query,
        operationParameters: {
          ...params,
          joins: canonicalizedJoins,
          count: canonicalizedCount,
          countDistinct: canonicalizedCountDistinct,
        },
      };
    }
    case "countRelations": {
      const params =
        query.operationParameters as CountRelationsQuery["operationParameters"];
      const canonicalizedJoins = canonicalizeJoinArray(
        params.joins,
        lookup,
        aliasTracker,
      );
      return {
        ...query,
        operationParameters: {
          ...params,
          joins: canonicalizedJoins,
          columns: params.columns.map((column) =>
            canonicalizeColumnName(lookup, baseTableName, column, aliasTracker),
          ),
          relationsToCount: params.relationsToCount.map((relation) => {
            if (!relation.distinct) {
              return relation;
            }
            if (relation.distinct.includes(".")) {
              return {
                ...relation,
                distinct: canonicalizeFullyQualifiedColumn(
                  relation.distinct,
                  lookup,
                  aliasTracker,
                ),
              };
            }
            const targetTable = resolveAliasTable(
              relation.name,
              baseTableName,
              canonicalizedJoins ?? [],
            );
            return {
              ...relation,
              distinct: canonicalizeColumnName(
                lookup,
                targetTable,
                relation.distinct,
                aliasTracker,
              ),
            };
          }),
        },
      };
    }
    case "aggregate": {
      const params =
        query.operationParameters as AggregateQuery["operationParameters"];
      const canonicalizedJoins = canonicalizeJoinArray(
        params.joins,
        lookup,
        aliasTracker,
      );
      const canonicalizeAggregateColumns = (columns: string[] | null) =>
        columns
          ? columns.map((column) =>
              canonicalizeMetricColumn(
                column,
                baseTableName,
                lookup,
                aliasTracker,
              ),
            )
          : null;
      return {
        ...query,
        operationParameters: {
          ...params,
          joins: canonicalizedJoins,
          avg: canonicalizeAggregateColumns(params.avg),
          sum: canonicalizeAggregateColumns(params.sum),
          min: canonicalizeAggregateColumns(params.min),
          max: canonicalizeAggregateColumns(params.max),
          count: canonicalizeAggregateColumns(params.count),
          median: canonicalizeAggregateColumns(params.median),
        },
      };
    }
    case "aggregateGroups": {
      const params =
        query.operationParameters as AggregateGroupsQuery["operationParameters"];
      const canonicalizedJoins = canonicalizeJoinArray(
        params.joins,
        lookup,
        aliasTracker,
      );
      const aliasMapping = new Map<string, string>();

      const canonicalizedGroupBy = params.groupBy.map((groupKey) => {
        if (groupKey.type === "column") {
          const canonicalizedColumn = canonicalizeFullyQualifiedColumn(
            groupKey.column,
            lookup,
            aliasTracker,
          );
          const defaultAlias = groupKey.column;
          const aliasInput = groupKey.alias ?? defaultAlias;
          const alias =
            aliasInput === defaultAlias ? canonicalizedColumn : aliasInput;
          aliasMapping.set(aliasInput, alias);
          aliasMapping.set(alias, alias);
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
            aliasTracker,
          );
          const defaultAlias = `${groupKey.column}|${groupKey.interval}`;
          const aliasInput = groupKey.alias ?? defaultAlias;
          const canonicalizedDefaultAlias = `${canonicalizedColumn}|${groupKey.interval}`;
          const alias =
            aliasInput === defaultAlias
              ? canonicalizedDefaultAlias
              : aliasInput;
          aliasMapping.set(aliasInput, alias);
          aliasMapping.set(alias, alias);
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
            aliasTracker,
          );
          const defaultAlias = `${groupKey.column}|${groupKey.component}`;
          const aliasInput = groupKey.alias ?? defaultAlias;
          const canonicalizedDefaultAlias = `${canonicalizedColumn}|${groupKey.component}`;
          const alias =
            aliasInput === defaultAlias
              ? canonicalizedDefaultAlias
              : aliasInput;
          aliasMapping.set(aliasInput, alias);
          aliasMapping.set(alias, alias);
          return {
            ...groupKey,
            column: canonicalizedColumn,
            alias,
          };
        }

        return groupKey;
      });

      const canonicalizeAgg = (
        aggregate: string[] | null | undefined,
      ): string[] | null => {
        if (!aggregate) return null;
        return aggregate.map((column) =>
          canonicalizeFullyQualifiedColumn(column, lookup, aliasTracker),
        );
      };

      const canonicalizeHaving = (
        having: AggregateGroupsQuery["operationParameters"]["having"],
      ): AggregateGroupsQuery["operationParameters"]["having"] => {
        if (!having) return having;
        return having.map((condition) => {
          if (condition.type === "aggregate") {
            return {
              ...condition,
              column: canonicalizeFullyQualifiedColumn(
                condition.column,
                lookup,
                aliasTracker,
              ),
            };
          }
          return {
            ...condition,
            key: aliasMapping.get(condition.key) ?? condition.key,
          };
        });
      };

      return {
        ...query,
        operationParameters: {
          ...params,
          joins: canonicalizedJoins,
          groupBy: canonicalizedGroupBy,
          aggregates: {
            ...params.aggregates,
            count: canonicalizeAgg(params.aggregates.count),
            countDistinct: canonicalizeAgg(params.aggregates.countDistinct),
            sum: canonicalizeAgg(params.aggregates.sum),
            min: canonicalizeAgg(params.aggregates.min),
            max: canonicalizeAgg(params.aggregates.max),
            avg: canonicalizeAgg(params.aggregates.avg),
          },
          having: canonicalizeHaving(params.having),
        },
      };
    }
    case "groupBy": {
      const params =
        query.operationParameters as GroupByQuery["operationParameters"];
      const canonicalizeAgg = (aggregate: string[] | null): string[] | null => {
        if (!aggregate) return null;
        return aggregate.map((column) =>
          canonicalizeFullyQualifiedColumn(column, lookup, aliasTracker),
        );
      };

      const aliasMapping = new Map<string, string>();

      const canonicalizedGroupBy = params.groupBy.map((groupKey) => {
        if (groupKey.type === "column") {
          const canonicalizedColumn = canonicalizeFullyQualifiedColumn(
            groupKey.column,
            lookup,
            aliasTracker,
          );
          const defaultAlias = groupKey.column;
          const aliasInput = groupKey.alias ?? defaultAlias;
          const alias =
            aliasInput === defaultAlias ? canonicalizedColumn : aliasInput;
          aliasMapping.set(aliasInput, alias);
          aliasMapping.set(alias, alias);
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
            aliasTracker,
          );
          const defaultAlias = `${groupKey.column}|${groupKey.interval}`;
          const aliasInput = groupKey.alias ?? defaultAlias;
          const canonicalizedDefaultAlias = `${canonicalizedColumn}|${groupKey.interval}`;
          const alias =
            aliasInput === defaultAlias
              ? canonicalizedDefaultAlias
              : aliasInput;
          aliasMapping.set(aliasInput, alias);
          aliasMapping.set(alias, alias);
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
            aliasTracker,
          );
          const defaultAlias = `${groupKey.column}|${groupKey.component}`;
          const aliasInput = groupKey.alias ?? defaultAlias;
          const canonicalizedDefaultAlias = `${canonicalizedColumn}|${groupKey.component}`;
          const alias =
            aliasInput === defaultAlias
              ? canonicalizedDefaultAlias
              : aliasInput;
          aliasMapping.set(aliasInput, alias);
          aliasMapping.set(alias, alias);
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
          ? {
              ...params.orderBy,
              key: aliasMapping.get(params.orderBy.key) ?? params.orderBy.key,
            }
          : {
              ...params.orderBy,
              column: canonicalizeFullyQualifiedColumn(
                params.orderBy.column,
                lookup,
                aliasTracker,
              ),
            };

      const canonicalizeHaving = (
        having: GroupByQuery["operationParameters"]["having"],
      ): GroupByQuery["operationParameters"]["having"] => {
        if (!having) return having;
        return having.map((condition) => {
          if (condition.type === "aggregate") {
            return {
              ...condition,
              column: canonicalizeFullyQualifiedColumn(
                condition.column,
                lookup,
                aliasTracker,
              ),
            };
          }
          return {
            ...condition,
            key: aliasMapping.get(condition.key) ?? condition.key,
          };
        });
      };

      return {
        ...query,
        operationParameters: {
          ...params,
          groupBy: canonicalizedGroupBy,
          count: canonicalizeAgg(params.count),
          countDistinct: canonicalizeAgg(params.countDistinct),
          sum: canonicalizeAgg(params.sum),
          min: canonicalizeAgg(params.min),
          max: canonicalizeAgg(params.max),
          avg: canonicalizeAgg(params.avg),
          orderBy: canonicalizedOrderBy,
          having: canonicalizeHaving(params.having),
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
  aliasTracker?: ColumnAliasMap,
) {
  const canonical = lookup.get(tableName)?.get(columnName) ?? columnName;
  recordAlias(aliasTracker, tableName, columnName, canonical);
  return canonical;
}

function canonicalizeFullyQualifiedColumn(
  column: string,
  lookup: ColumnLookup,
  aliasTracker?: ColumnAliasMap,
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
    aliasTracker,
  );
  return `${tableName}.${canonicalizedColumn}`;
}

type CanonicalizableJoin = {
  table: string;
  name?: string;
  path: JoinPathHop[];
  joinType?: string;
};

function canonicalizeJoinArray<T extends CanonicalizableJoin>(
  joins: T[] | null | undefined,
  lookup: ColumnLookup,
  aliasTracker?: ColumnAliasMap,
): T[] | null | undefined {
  if (!joins) {
    return joins;
  }

  return joins.map((join) => ({
    ...join,
    path: join.path.map((hop) => ({
      source: hop.source.map((column) =>
        canonicalizeFullyQualifiedColumn(column, lookup, aliasTracker),
      ),
      target: hop.target.map((column) =>
        canonicalizeFullyQualifiedColumn(column, lookup, aliasTracker),
      ),
    })),
  }));
}

function resolveLookupTableName(lookup: ColumnLookup, alias: string) {
  if (lookup.has(alias)) {
    return alias;
  }

  const segments = alias.split(".");
  for (let index = segments.length - 1; index >= 0; index--) {
    const candidate = segments[index];
    if (candidate && lookup.has(candidate)) {
      return candidate;
    }
  }

  return alias;
}

function canonicalizeMetricColumn(
  column: string,
  baseTableName: string,
  lookup: ColumnLookup,
  aliasTracker?: ColumnAliasMap,
) {
  if (column === "_all") return column;

  const separatorIndex = column.lastIndexOf(".");
  if (separatorIndex === -1) {
    throw new Error(
      `Column ${column} must be qualified as tableOrAlias.column before canonicalization.`,
    );
  }

  const alias = column.slice(0, separatorIndex);
  const columnName = column.slice(separatorIndex + 1);
  const lookupTable = resolveLookupTableName(lookup, alias);
  const canonicalColumn = canonicalizeColumnName(
    lookup,
    lookupTable,
    columnName,
    aliasTracker,
  );
  return `${alias}.${canonicalColumn}`;
}

function getTableSchema(schema: Schema, tableName: string) {
  return schema.find((table) => table.name === tableName);
}

type JoinAliasMap = Map<string, string>;

function buildJoinPathMap(
  baseTableName: string,
  joins: FindManyQuery["operationParameters"]["joins"],
): JoinAliasMap {
  const map: JoinAliasMap = new Map();
  map.set(baseTableName, baseTableName);

  (joins ?? []).forEach((join) => {
    const alias = join.name ?? join.table;
    if (!map.has(alias)) {
      map.set(alias, join.table);
    }
    const segments = alias.split(".");
    const terminal = segments[segments.length - 1];
    if (terminal && !map.has(terminal)) {
      map.set(terminal, join.table);
    }
  });

  return map;
}

function canonicalizeColumnForJoinPath(
  lookup: ColumnLookup,
  joinAliasMap: JoinAliasMap,
  baseTableName: string,
  path: string,
  column: string,
  aliasTracker?: ColumnAliasMap,
) {
  if (column.includes(".")) {
    return canonicalizeFullyQualifiedColumn(column, lookup, aliasTracker);
  }

  const candidateAliases = [
    path,
    `${baseTableName}.${path}`,
    path.split(".").pop(),
    baseTableName,
  ].filter((alias): alias is string => Boolean(alias));

  for (const alias of candidateAliases) {
    const tableName = joinAliasMap.get(alias);
    if (tableName) {
      return canonicalizeColumnName(lookup, tableName, column, aliasTracker);
    }
  }

  return canonicalizeColumnName(lookup, baseTableName, column, aliasTracker);
}

function resolveAliasTable(
  alias: string,
  baseTableName: string,
  joins: Array<{ table: string; name?: string }> | null | undefined,
): string {
  if (!joins?.length || alias === baseTableName) {
    return baseTableName;
  }

  const direct = joins.find((join) => (join.name ?? join.table) === alias);
  if (direct) {
    return direct.table;
  }

  const suffixMatch = joins.find((join) =>
    (join.name ?? join.table).endsWith(`.${alias}`),
  );
  if (suffixMatch) {
    return suffixMatch.table;
  }

  return alias;
}
