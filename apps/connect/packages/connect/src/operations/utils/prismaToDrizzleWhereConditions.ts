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
  inArray,
  exists,
  isNull,
  isNotNull,
  like,
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

function parseToManyRelationFilter({
  drizzleSchema,
  currentTable,
  currentTableName,
  relationName,
  filterObj,
  computedColumns,
}: {
  drizzleSchema: Record<string, any>;
  currentTable: Table;
  currentTableName: string;
  relationName: string;
  filterObj: Record<"some" | "none" | "every", any>;
  computedColumns: ComputedColumn[];
}): SQL {
  const [[operator, nestedCondition]] = Object.entries(filterObj);

  // ────────── find the related table and FK/PK pair ──────────
  const relatedTableName = getRelatedTableNameFromPath(
    [currentTableName, relationName],
    drizzleSchema
  );
  const relatedTable = drizzleSchema[relatedTableName];

  const [currentKey, relatedKey] = findRelationsBetweenTables(
    currentTableName,
    relatedTableName,
    relationName,
    drizzleSchema
  );

  // ────────── build the nested condition (if any) ──────────
  const hasNested = nestedCondition && Object.keys(nestedCondition).length > 0;
  const nestedSql = hasNested
    ? parseCondition({
        drizzleSchema,
        table: relatedTable,
        tableName: relatedTableName,
        condition: nestedCondition,
        columns: relatedTable,
        computedColumns,
      })
    : undefined;

  // base predicate that connects parent ↔ child
  const pkFkPredicate = eq(relatedTable[relatedKey], currentTable[currentKey]);

  // --------- construct the sub‑query we will later pipe into EXISTS ----------
  const subQueryBody = nestedSql
    ? and(pkFkPredicate, nestedSql)
    : pkFkPredicate;

  const subQuery = sql`(
    SELECT 1
    FROM   ${relatedTable}
    WHERE  ${subQueryBody})
  `;

  // ───────────────────────── operator semantics ──────────────────────────────
  switch (operator) {
    case "some":
      // at least one child row satisfies nestedSql
      return exists(subQuery);

    case "none":
      // no child row satisfies nestedSql
      return notExists(subQuery);

    case "every": {
      // “every child row satisfies nestedSql”
      // ≡ “no child row violates nestedSql”
      const violation = nestedSql ? sql`NOT (${nestedSql})` : sql`FALSE`;
      const subQueryEvery = sql`(
        SELECT 1
        FROM   ${relatedTable}
        WHERE  ${and(pkFkPredicate, violation)}
      )`;
      return notExists(subQueryEvery);
    }

    default:
      throw new Error(
        `Unknown to-many operator "${operator}". Allowed: some | none | every`
      );
  }
}

function parseToOneRelationFilter({
  drizzleSchema,
  currentTable,
  currentTableName,
  relationName,
  filterObj,
  computedColumns,
}: {
  drizzleSchema: Record<string, any>;
  currentTable: Record<string, any>;
  currentTableName: string;
  relationName: string;
  filterObj: { is?: any; isNot?: any };
  computedColumns: ComputedColumn[];
}): SQL {
  const [[operator, nestedCondition]] = Object.entries(filterObj) as [
    ["is" | "isNot", any]
  ];

  // lookup related table
  const relatedTableName = getRelatedTableNameFromPath(
    [currentTableName, relationName],
    drizzleSchema
  );
  const relatedTable = drizzleSchema[relatedTableName];

  // find FK/PK between current (child) and related (parent)
  const [currentKey, relatedKey] = findRelationsBetweenTables(
    currentTableName,
    relatedTableName,
    relationName,
    drizzleSchema
  );

  // nestedCondition === null means user wrote is: null or isNot: null
  if (nestedCondition === null) {
    if (operator === "is") {
      // absence of relation
      return eq(currentTable[currentKey], null);
    } else {
      // presence of relation
      return ne(currentTable[currentKey], null);
    }
  }

  // turn nestedCondition into SQL if it has keys
  const nestedFilterSql: SQL | undefined =
    nestedCondition && Object.keys(nestedCondition).length > 0
      ? parseCondition({
          drizzleSchema,
          table: relatedTable,
          tableName: relatedTableName,
          condition: nestedCondition,
          columns: relatedTable,
          computedColumns,
        })
      : undefined;

  if (operator === "is") {
    // if no nested filters, simply require fk != null
    if (!nestedFilterSql) {
      return ne(currentTable[currentKey], null);
    }
    // otherwise require a matching parent row
    const sub = sql`
      SELECT 1 
      FROM ${relatedTable}
      WHERE ${relatedTable[relatedKey]} = ${currentTable[currentKey]}
        AND ${nestedFilterSql}
    `;
    return exists(sql`(${sub})`);
  } else {
    // isNot: no matching parent row with those properties
    if (!nestedFilterSql) {
      return eq(currentTable[currentKey], null);
    }
    const sub = sql`
      SELECT 1 
      FROM ${relatedTable}
      WHERE ${relatedTable[relatedKey]} = ${currentTable[currentKey]}
        AND ${nestedFilterSql}
    `;
    return notExists(sql`(${sub})`);
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
  computedColumns: ComputedColumn[];
}): SQL {
  // 1) filterObj === null  → column IS NULL or "no to‑one" relation
  if (filterObj === null) {
    // scalar column?
    if (columnName in columns) {
      return eq(columns[columnName], null);
    }
    // otherwise it's a to-one relation absence
    const [currentKey] = findRelationsBetweenTables(
      tableName,
      getRelatedTableNameFromPath([tableName, columnName], drizzleSchema),
      columnName,
      drizzleSchema
    );
    return eq(table[currentKey], null);
  }

  // 2) empty object {}  → treat as "no to‑one" relation
  if (
    typeof filterObj === "object" &&
    !Array.isArray(filterObj) &&
    Object.keys(filterObj).length === 0 &&
    !(columnName in columns)
  ) {
    const [currentKey] = findRelationsBetweenTables(
      tableName,
      getRelatedTableNameFromPath([tableName, columnName], drizzleSchema),
      columnName,
      drizzleSchema
    );
    return eq(table[currentKey], null);
  }

  const subExpressions: SQL[] = [];

  // 3) iterate operators
  for (const [operator, value] of Object.entries(filterObj)) {
    // to‑many relations
    if (operator === "some" || operator === "none" || operator === "every") {
      subExpressions.push(
        parseToManyRelationFilter({
          drizzleSchema,
          currentTable: table,
          currentTableName: tableName,
          relationName: columnName,
          filterObj: filterObj as Record<"some" | "none" | "every", any>,
          computedColumns: computedColumns,
        })
      );
      // only one of some/none/every per relation, so we can break
      continue;
    }

    // to‑one relations
    if (operator === "is" || operator === "isNot") {
      subExpressions.push(
        parseToOneRelationFilter({
          drizzleSchema,
          currentTable: table,
          currentTableName: tableName,
          relationName: columnName,
          filterObj: { [operator]: value },
          computedColumns: computedColumns,
        })
      );
      continue;
    }

    // scalar columns
    let columnExpr: SQL | undefined = columns[columnName];
    if (!columnExpr) {
      columnExpr = getColumnFromTable({
        columnName,
        tableName,
        drizzleSchema,
        computedColumns,
      }) as SQL;
    }

    switch (operator) {
      case "equals":
        if (value === null) {
          subExpressions.push(isNull(columnExpr));
          break;
        }
        subExpressions.push(eq(columnExpr, value));
        break;
      case "gt":
        subExpressions.push(gt(columnExpr, value));
        break;
      case "gte":
        subExpressions.push(gte(columnExpr, value));
        break;
      case "lt":
        subExpressions.push(lt(columnExpr, value));
        break;
      case "lte":
        subExpressions.push(lte(columnExpr, value));
        break;
      case "not":
        if (value === null) {
          subExpressions.push(isNotNull(columnExpr));
          break;
        }
        subExpressions.push(ne(columnExpr, value));
        break;
      case "in":
        subExpressions.push(inArray(columnExpr, value));
        break;
      case "contains":
        subExpressions.push(like(columnExpr, `%${value}%`));
        break;
      case "contains_insensitive":
        subExpressions.push(
          like(sql`lower(${columnExpr})`, `%${value.toLowerCase()}%`)
        );
        break;
      default:
        throw new Error(
          `Unsupported operator "${operator}" in filter for "${columnName}". ` +
            `Allowed operators: equals, gt, gte, lt, lte, not, in, contains, contains_insensitive, some, none, every, is, isNot.`
        );
    }
  }

  if (subExpressions.length === 0) {
    throw new Error(`No valid operators found in filter for "${columnName}".`);
  }

  return subExpressions.length === 1
    ? subExpressions[0]
    : (and(...subExpressions) as SQL);
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
