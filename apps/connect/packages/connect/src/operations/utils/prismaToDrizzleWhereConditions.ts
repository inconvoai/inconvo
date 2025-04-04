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
import type { ComputedColumn, WhereConditions } from "~/types/querySchema";
import { findRelationsBetweenTables } from "~/operations/utils/findRelationsBetweenTables";
import { getRelatedTableNameFromPath } from "./drizzleSchemaHelpers";
import { getColumnFromTable } from "./getColumnFromTable";

// -----------------------------------------------------------------------------
// Types & Constants
// -----------------------------------------------------------------------------

type Table = Record<string, any>;
type FilterObject = Record<string, any>;

// -----------------------------------------------------------------------------
// Parsing To-Many Relation Filters
// -----------------------------------------------------------------------------

/**
 * Builds a sub-query condition for "to-many" relations (e.g., "none", "some", "every").
 * Currently this example only supports "none", but can be extended.
 */
function parseToManyRelationFilter({
  drizzleSchema,
  currentTable,
  currentTableName,
  relationName,
  filterObj,
}: {
  drizzleSchema: Record<string, any>;
  currentTable: Table;
  currentTableName: string;
  relationName: string;
  filterObj: FilterObject;
}): SQL {
  const [operator, _nestedCondition] = Object.entries(filterObj)[0];

  const relatedTableName = getRelatedTableNameFromPath(
    [currentTableName, relationName],
    drizzleSchema
  );

  const [currentKey, relatedKey, groupBy] = findRelationsBetweenTables(
    currentTableName,
    relatedTableName,
    relationName,
    drizzleSchema
  );

  switch (operator) {
    case "none": {
      const baseSubquery = sql`
        SELECT 1 FROM ${drizzleSchema[relatedTableName]}
        WHERE ${drizzleSchema[relatedTableName][relatedKey]} = ${currentTable[currentKey]}
        AND ${drizzleSchema[relatedTableName][relatedKey]} IS NOT NULL`;
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
  drizzleSchema,
  table,
  tableName,
  columnName,
  filterObj,
  columns,
  computedColumns,
}: {
  drizzleSchema: Record<string, Table | Relation>;
  table: Table;
  tableName: string;
  columnName: string;
  filterObj: FilterObject;
  columns: Record<string, any>;
  computedColumns?: ComputedColumn[];
}): SQL {
  const subExpressions: SQL[] = [];

  for (const [operator, value] of Object.entries(filterObj)) {
    // Check if this is a "to-many" operator like "none"/"some"/"every"
    if (operator === "none") {
      subExpressions.push(
        parseToManyRelationFilter({
          drizzleSchema,
          currentTable: table,
          currentTableName: tableName,
          relationName: columnName,
          filterObj,
        })
      );
      continue;
    }

    let column: SQL | undefined;
    for (const [key, value] of Object.entries(columns)) {
      if (key === columnName) {
        column = value;
        break;
      }
    }
    if (!column) {
      column = getColumnFromTable({
        columnName,
        tableName,
        drizzleSchema,
        computedColumns,
      }) as SQL;
    }

    switch (operator) {
      case "equals":
        subExpressions.push(eq(column, value));
        break;
      case "gt":
        subExpressions.push(gt(column, value));
        break;
      case "gte":
        subExpressions.push(gte(column, value));
        break;
      case "lt":
        subExpressions.push(lt(column, value));
        break;
      case "lte":
        subExpressions.push(lte(column, value));
        break;
      case "not":
        subExpressions.push(ne(column, value));
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
  drizzleSchema,
  table,
  tableName,
  conditionObject,
  columns,
  computedColumns,
}: {
  drizzleSchema: Record<string, Table | Relation>;
  table: Table;
  tableName: string;
  conditionObject: FilterObject;
  columns: Record<string, any>;
  computedColumns: ComputedColumn[];
}): SQL {
  const expressions = Object.entries(conditionObject).map(
    ([columnName, filterObj]) =>
      parseColumnFilter({
        drizzleSchema,
        table,
        tableName,
        columnName,
        filterObj,
        columns,
        computedColumns,
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
  drizzleSchema,
  table,
  tableName,
  condition,
  columns,
  computedColumns,
}: {
  drizzleSchema: Record<string, Table | Relation>;
  table: Table;
  tableName: string;
  condition: any;
  columns: Record<string, any>;
  computedColumns: ComputedColumn[];
}): SQL {
  // If it's an array, interpret it as an implicit AND of multiple objects
  if (Array.isArray(condition)) {
    const andClauses = condition.map((c) =>
      parseCondition({
        drizzleSchema,
        table,
        tableName,
        condition: c,
        columns,
        computedColumns,
      })
    );
    return and(...andClauses) as SQL;
  }

  if (condition && typeof condition === "object") {
    // Handle Prisma's "OR"
    if (condition.OR) {
      const orClauses = condition.OR.map((c: any) =>
        parseCondition({
          drizzleSchema,
          table,
          tableName,
          condition: c,
          columns,
          computedColumns,
        })
      );
      return or(...orClauses) as SQL;
    }
    // Handle Prisma's "AND"
    if (condition.AND) {
      const andClauses = condition.AND.map((c: any) =>
        parseCondition({
          drizzleSchema,
          table,
          tableName,
          condition: c,
          columns,
          computedColumns,
        })
      );
      return and(...andClauses) as SQL;
    }
    // Handle Prisma's "NOT"
    if (condition.NOT) {
      const notClauses = condition.NOT.map((c: any) =>
        parseCondition({
          drizzleSchema,
          table,
          tableName,
          condition: c,
          columns,
          computedColumns,
        })
      );
      const mergedNot =
        notClauses.length > 1 ? and(...notClauses) : notClauses[0];
      return not(mergedNot);
    }

    // If it's a plain object with columns => parse it as a single-condition
    return parseSingleCondition({
      drizzleSchema,
      table,
      tableName,
      conditionObject: condition,
      columns,
      computedColumns,
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
  drizzleSchema,
  columns,
  tableName,
  where,
  computedColumns,
}: {
  drizzleSchema: Record<string, Table | Relation>;
  columns: Record<string, any>;
  tableName: string;
  where: WhereConditions;
  computedColumns: ComputedColumn[] | undefined;
}): SQL | undefined {
  // If "where" is empty/undefined, return nothing to skip applying a WHERE clause.
  if (!where || (Array.isArray(where) && where.length === 0)) {
    return undefined;
  }

  return parseCondition({
    drizzleSchema,
    table: drizzleSchema[tableName],
    condition: where,
    tableName,
    columns,
    computedColumns: computedColumns ?? [],
  });
}
