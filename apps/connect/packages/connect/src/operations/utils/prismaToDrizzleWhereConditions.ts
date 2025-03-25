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
  SQL,
  Relation,
} from "drizzle-orm";
import type { WhereConditions } from "~/types/querySchema";
import { findRelationsBetweenTables } from "~/operations/utils/findRelationsBetweenTables";

// -----------------------------------------------------------------------------
// Types & Constants
// -----------------------------------------------------------------------------

type Table = Record<string, any>;
type FilterObject = Record<string, any>;

/**
 * Determines if a given value is a valid ISO8601 date string.
 */
function isDateString(val: unknown): boolean {
  const iso8601Regex = /^\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}\.\d{3}Z$/;
  return typeof val === "string" && iso8601Regex.test(val);
}

// -----------------------------------------------------------------------------
// Parsing Date Operators
// -----------------------------------------------------------------------------

/**
 * Builds a SQL fragment for comparison operators against a date column.
 */
function parseDateOperator(
  table: Table,
  columnName: string,
  operator: string,
  value: any
): SQL {
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
        `Unsupported date operator "${operator}" on "${columnName}". ` +
          `Allowed: equals, gt, gte, lt, lte, not.`
      );
  }
}

// -----------------------------------------------------------------------------
// Parsing To-Many Relation Filters
// -----------------------------------------------------------------------------

/**
 * Builds a sub-query condition for "to-many" relations (e.g., "none", "some", "every").
 * Currently this example only supports "none", but can be extended.
 */
function parseToManyRelationFilter({
  tableSchemas,
  currentTable,
  currentTableName,
  relationName,
  filterObj,
}: {
  // tableSchemas: Record<string, Table | Relation>;
  tableSchemas: Record<string, any>;
  currentTable: Table;
  currentTableName: string;
  relationName: string;
  filterObj: FilterObject;
}): SQL {
  const [operator, _nestedCondition] = Object.entries(filterObj)[0];

  // TODO: Update after updating findRelationsBetweenTables
  const [currentKey, relatedKey] = findRelationsBetweenTables(
    tableSchemas[currentTableName],
    tableSchemas[relationName]
  );

  switch (operator) {
    case "none": {
      const baseSubquery = sql`
        SELECT 1 FROM ${tableSchemas[relationName]}
        WHERE ${tableSchemas[relationName][relatedKey]} = ${currentTable[currentKey]}
        AND ${tableSchemas[relationName][relatedKey]} IS NOT NULL`;

      // TODO: If there's a nested condition, apply it to the subquery
      // ATM nested conditions are not supported
      // if there are passed zod will parse them out

      return notExists(sql`(${baseSubquery})`);
    }

    default:
      throw new Error(
        `Unsupported operator "${operator}" in filter for relation "${relationName}". ` +
          `Currently supported: none.`
      );
  }
}

// -----------------------------------------------------------------------------
// Parsing Column Filters
// -----------------------------------------------------------------------------

/**
 * Builds a combined SQL expression for all operators on a single column/relation.
 * Example of filterObj: { gt: 3, lt: 10 } => AND(gt(column, 3), lt(column, 10)).
 */
function parseColumnFilter({
  tableSchemas,
  table,
  tableName,
  columnName,
  filterObj,
}: {
  tableSchemas: Record<string, Table | Relation>;
  table: Table;
  tableName: string;
  columnName: string;
  filterObj: FilterObject;
}): SQL {
  const subExpressions: SQL[] = [];

  for (const [operator, value] of Object.entries(filterObj)) {
    // Check if this is a "to-many" operator like "none"/"some"/"every"
    if (operator === "none") {
      subExpressions.push(
        parseToManyRelationFilter({
          tableSchemas,
          currentTable: table,
          currentTableName: tableName,
          relationName: columnName,
          filterObj,
        })
      );
      continue;
    }

    // If it's a date, use special date comparison syntax
    if (isDateString(value)) {
      subExpressions.push(
        parseDateOperator(table, columnName, operator, value)
      );
      continue;
    }

    // Otherwise, handle standard drizzle-orm comparisons
    switch (operator) {
      case "equals":
        subExpressions.push(eq(table[columnName], value));
        break;
      case "gt":
        subExpressions.push(gt(table[columnName], value));
        break;
      case "gte":
        subExpressions.push(gte(table[columnName], value));
        break;
      case "lt":
        subExpressions.push(lt(table[columnName], value));
        break;
      case "lte":
        subExpressions.push(lte(table[columnName], value));
        break;
      case "not":
        subExpressions.push(ne(table[columnName], value));
        break;
      default:
        throw new Error(
          `Unsupported operator "${operator}" in filter for "${columnName}". ` +
            `Allowed operators: equals, gt, gte, lt, lte, not, none.`
        );
    }
  }

  // If no valid sub-expressions were created, the user likely provided an empty/invalid filter
  if (subExpressions.length === 0) {
    throw new Error(`No valid operators found in filter for "${columnName}".`);
  }

  // Combine multiple operators for the same column with AND
  if (subExpressions.length === 1) {
    return subExpressions[0];
  } else {
    return and(...subExpressions) as SQL;
  }
}

// -----------------------------------------------------------------------------
// High-Level Condition Parsing (AND/OR/NOT, etc.)
// -----------------------------------------------------------------------------

/**
 * Handles the case where a single object has multiple columns to filter:
 * {
 *   someColumn: { gt: 10, lt: 20 },
 *   anotherCol: { equals: "foo" }
 * }
 */
function parseSingleCondition({
  tableSchemas,
  table,
  tableName,
  conditionObject,
}: {
  tableSchemas: Record<string, Table | Relation>;
  table: Table;
  tableName: string;
  conditionObject: FilterObject;
}): SQL {
  const expressions = Object.entries(conditionObject).map(
    ([columnName, filterObj]) =>
      parseColumnFilter({
        tableSchemas,
        table,
        tableName,
        columnName,
        filterObj,
      })
  );

  // Combine all column-filters with an AND
  return expressions.length > 1 ? (and(...expressions) as SQL) : expressions[0];
}

/**
 * Recursively parses a nested condition object which may contain:
 * - AND: []
 * - OR: []
 * - NOT: []
 * or be a straightforward field operator set.
 */
function parseCondition({
  tableSchemas,
  table,
  tableName,
  condition,
}: {
  tableSchemas: Record<string, Table | Relation>;
  table: Table;
  tableName: string;
  condition: any;
}): SQL {
  // If it's an array, interpret it as an implicit AND of multiple objects
  if (Array.isArray(condition)) {
    const andClauses = condition.map((c) =>
      parseCondition({
        tableSchemas,
        table,
        tableName,
        condition: c,
      })
    );
    return and(...andClauses) as SQL;
  }

  if (condition && typeof condition === "object") {
    // Handle Prisma's "OR"
    if (condition.OR) {
      const orClauses = condition.OR.map((c: any) =>
        parseCondition({
          tableSchemas,
          table,
          tableName,
          condition: c,
        })
      );
      return or(...orClauses) as SQL;
    }
    // Handle Prisma's "AND"
    if (condition.AND) {
      const andClauses = condition.AND.map((c: any) =>
        parseCondition({
          tableSchemas,
          table,
          tableName,
          condition: c,
        })
      );
      return and(...andClauses) as SQL;
    }
    // Handle Prisma's "NOT"
    if (condition.NOT) {
      const notClauses = condition.NOT.map((c: any) =>
        parseCondition({
          tableSchemas,
          table,
          tableName,
          condition: c,
        })
      );
      const mergedNot =
        notClauses.length > 1 ? and(...notClauses) : notClauses[0];
      return not(mergedNot);
    }

    // If it's a plain object with columns => parse it as a single-condition
    return parseSingleCondition({
      tableSchemas,
      table,
      tableName,
      conditionObject: condition,
    });
  }

  throw new Error(`Unsupported condition format: ${JSON.stringify(condition)}`);
}

// -----------------------------------------------------------------------------
// Public Entry Function
// -----------------------------------------------------------------------------

/**
 * Main entry point for converting a Prisma-like "where" object
 * into a drizzle-orm compatible SQL condition.
 */
export function parsePrismaWhere({
  tableSchemas,
  tableName,
  where,
}: {
  tableSchemas: Record<string, Table | Relation>;
  tableName: string;
  where: WhereConditions;
}): SQL | undefined {
  // If "where" is empty/undefined, return nothing to skip applying a WHERE clause.
  if (!where || (Array.isArray(where) && where.length === 0)) {
    return undefined;
  }
  return parseCondition({
    tableSchemas,
    table: tableSchemas[tableName],
    tableName,
    condition: where,
  });
}
