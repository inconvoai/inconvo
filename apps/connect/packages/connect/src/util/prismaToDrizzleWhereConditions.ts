import {
  eq,
  ne,
  gt,
  gte,
  lt,
  lte,
  and,
  or,
  not,
  sql,
  notExists,
} from "drizzle-orm";
import * as drizzleTables from "~/../drizzle/schema";
import { findRelationsBetweenTables } from "~/util/findRelationsBetweenTables";
const tables: Record<string, any> = drizzleTables;

type Table = Record<string, any>;
type FilterObject = Record<string, any>;

function parseToManyRelationFilter(
  table: Table,
  tableName: string,
  columnName: string,
  filterObj: FilterObject
) {
  const [operator, value] = Object.entries(filterObj)[0];

  const [currentTableKey, relatedTableKey] = findRelationsBetweenTables(
    tables[tableName],
    tables[columnName]
  );

  switch (operator) {
    case "none":
      return notExists(
        sql`(SELECT ${tables[columnName][relatedTableKey]} FROM ${tables[columnName]} WHERE ${tables[columnName][relatedTableKey]} = ${table[currentTableKey]} AND ${tables[columnName][relatedTableKey]} IS NOT NULL)`
      );
    default:
      throw new Error(
        `Unsupported operator "${operator}" in filter for "${columnName}". Supported operators: none.`
      );
  }
}

function parseDateFilter(
  table: Table,
  columnName: string,
  filterObj: FilterObject
) {
  const [operator, value] = Object.entries(filterObj)[0];

  switch (operator) {
    case "equals":
      return sql`${table[columnName]} = ${value}::timestamp`;
    case "gt":
      return sql`${table[columnName]} > ${value}::timestamp`;
    case "gte":
      return sql`${table[columnName]} >= ${value}::timestamp`;
    case "lt":
      return sql`${table[columnName]} < ${value}::timestamp`;
    case "lte":
      return sql`${table[columnName]} <= ${value}::timestamp`;
    case "not":
      return sql`${table[columnName]} != ${value}::timestamp`;
    default:
      throw new Error(
        `Unsupported operator "${operator}" in filter for "${columnName}". Supported operators: equals, gt, gte, lt, lte, not.`
      );
  }
}

function isDateString(val: any): boolean {
  const iso8601Regex = /^\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}\.\d{3}Z$/;
  return typeof val === "string" && iso8601Regex.test(val);
}

function parseColumnFilter(
  table: Table,
  tableName: string,
  columnName: string,
  filterObj: FilterObject
) {
  const [operator, value] = Object.entries(filterObj)[0];

  if (isDateString(value)) {
    return parseDateFilter(table, columnName, filterObj);
  }

  switch (operator) {
    case "equals":
      return eq(table[columnName], value);
    case "gt":
      return gt(table[columnName], value);
    case "gte":
      return gte(table[columnName], value);
    case "lt":
      return lt(table[columnName], value);
    case "lte":
      return lte(table[columnName], value);
    case "not":
      return ne(table[columnName], value);
    case "none":
      return parseToManyRelationFilter(table, tableName, columnName, filterObj);
    default:
      throw new Error(
        `Unsupported operator "${operator}" in filter for "${columnName}". Supported operators: equals, gt, gte, lt, lte, not, none.`
      );
  }
}

function parseSingleCondition(
  table: Table,
  tableName: string,
  conditionObject: FilterObject
) {
  const expressions = Object.entries(conditionObject).map(
    ([columnName, filterObj]) =>
      parseColumnFilter(table, tableName, columnName, filterObj)
  );
  return expressions.length > 1 ? and(...expressions) : expressions[0];
}

function parseCondition(
  table: Table,
  tableName: string,
  condition: any
): ReturnType<typeof and | typeof or | typeof not> {
  if (Array.isArray(condition)) {
    const andClauses = condition.map((c) =>
      parseCondition(table, tableName, c)
    );
    return and(...andClauses);
  }

  if (condition && typeof condition === "object") {
    if (condition.OR) {
      const orClauses = condition.OR.map((c: any) =>
        parseCondition(table, tableName, c)
      );
      return or(...orClauses);
    }
    if (condition.AND) {
      const andClauses = condition.AND.map((c: any) =>
        parseCondition(table, tableName, c)
      );
      return and(...andClauses);
    }
    if (condition.NOT) {
      const notClauses = condition.NOT.map((c: any) =>
        parseCondition(table, tableName, c)
      );
      const combined =
        notClauses.length > 1 ? and(...notClauses) : notClauses[0];
      return not(combined);
    }

    return parseSingleCondition(table, tableName, condition);
  }

  throw new Error(`Unsupported condition format: ${JSON.stringify(condition)}`);
}

export function parsePrismaWhere(
  table: Table,
  tableName: string,
  where: any
): ReturnType<typeof parseCondition> | undefined {
  if (!where || (Array.isArray(where) && where.length === 0)) {
    return undefined;
  }
  return parseCondition(table, tableName, where);
}
