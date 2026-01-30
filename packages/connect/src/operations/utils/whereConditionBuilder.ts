import { sql } from "kysely";
import type { Expression, SqlBool } from "kysely";
import type {
  WhereConditions,
  TableConditionsMap,
} from "../../types/querySchema";
import { getColumnFromTable } from "./computedColumns";
import { getTableIdentifier } from "./tableIdentifier";
import type { SchemaResponse } from "../../types/types";
import type { DatabaseDialect } from "../types";

type _FilterObject = Record<string, unknown>;

// Helper function to combine SQL expressions with AND/OR operators
// Avoids the RawBuilder await issue by using a loop instead of reduce
function combineSqlExpressions(
  expressions: Expression<SqlBool>[],
  operator: "AND" | "OR",
): Expression<SqlBool> {
  if (expressions.length === 0) {
    throw new Error("Cannot combine empty array of expressions");
  }
  if (expressions.length === 1) {
    return expressions[0]!;
  }

  let result = expressions[0]!;
  for (let i = 1; i < expressions.length; i++) {
    const prevResult = result;
    const nextExpr = expressions[i]!;
    if (operator === "AND") {
      result = sql`(${prevResult}) AND (${nextExpr})`;
    } else {
      result = sql`(${prevResult}) OR (${nextExpr})`;
    }
  }
  return result;
}

export function buildWhereConditions(
  whereAndArray: WhereConditions,
  tableName: string,
  schema: SchemaResponse,
  dialect: DatabaseDialect,
  tableConditions?: TableConditionsMap,
): Expression<SqlBool> | undefined {
  if (!whereAndArray || whereAndArray.length === 0) {
    return undefined;
  }

  // Process the whereAndArray as an implicit AND of all conditions
  const topLevelConditions: Expression<SqlBool>[] = [];

  for (let i = 0; i < whereAndArray.length; i++) {
    const whereGroup = whereAndArray[i];
    if (whereGroup === null || whereGroup === undefined) continue;

    const condition = parseConditionObject(
      whereGroup,
      tableName,
      schema,
      dialect,
      tableConditions,
    );
    if (condition) {
      topLevelConditions.push(condition);
    }
  }

  if (topLevelConditions.length === 0) {
    return undefined;
  }

  if (topLevelConditions.length === 1) {
    return topLevelConditions[0]!;
  }

  // Build AND expression without using reduce to avoid RawBuilder await issues
  let result = topLevelConditions[0]!;
  for (let i = 1; i < topLevelConditions.length; i++) {
    const prevResult = result;
    const nextCond = topLevelConditions[i]!;
    result = sql`(${prevResult}) AND (${nextCond})`;
  }
  return result;
}

function parseConditionObject(
  condition: any,
  tableName: string,
  schema: SchemaResponse,
  dialect: DatabaseDialect,
  tableConditions?: TableConditionsMap,
  insideRelationFilter: boolean = false,
): Expression<SqlBool> | undefined {
  if (!condition || typeof condition !== "object") {
    return undefined;
  }

  // Handle AND
  if ("AND" in condition && Array.isArray(condition.AND)) {
    const andConditions: Expression<SqlBool>[] = [];

    for (const subCondition of condition.AND) {
      const parsed = parseConditionObject(
        subCondition,
        tableName,
        schema,
        dialect,
        tableConditions,
        insideRelationFilter,
      );
      if (parsed !== undefined) {
        andConditions.push(parsed);
      }
    }

    if (andConditions.length === 0) return undefined;
    if (andConditions.length === 1) return andConditions[0]!;

    let result = andConditions[0]!;
    for (let i = 1; i < andConditions.length; i++) {
      const prevResult = result;
      const nextCond = andConditions[i]!;
      result = sql`(${prevResult}) AND (${nextCond})`;
    }
    return result;
  }

  // Handle OR
  if ("OR" in condition && Array.isArray(condition.OR)) {
    const orConditions: Expression<SqlBool>[] = [];

    for (const subCondition of condition.OR) {
      const parsed = parseConditionObject(
        subCondition,
        tableName,
        schema,
        dialect,
        tableConditions,
        insideRelationFilter,
      );
      if (parsed !== undefined) {
        orConditions.push(parsed);
      }
    }

    if (orConditions.length === 0) return undefined;
    if (orConditions.length === 1) return orConditions[0]!;

    let result = orConditions[0]!;
    for (let i = 1; i < orConditions.length; i++) {
      const prevResult = result;
      const nextCond = orConditions[i]!;
      result = sql`(${prevResult}) OR (${nextCond})`;
    }
    return sql`(${result})`;
  }

  // Handle NOT
  if ("NOT" in condition && Array.isArray(condition.NOT)) {
    const notConditions: Expression<SqlBool>[] = [];

    for (const subCondition of condition.NOT) {
      const parsed = parseConditionObject(
        subCondition,
        tableName,
        schema,
        dialect,
        tableConditions,
        insideRelationFilter,
      );
      if (parsed !== undefined) {
        notConditions.push(parsed);
      }
    }

    if (notConditions.length === 0) return undefined;

    let combined: Expression<SqlBool>;
    if (notConditions.length === 1) {
      combined = notConditions[0]!;
    } else {
      let result = notConditions[0]!;
      for (let i = 1; i < notConditions.length; i++) {
        const prevResult = result;
        const nextCond = notConditions[i]!;
        result = sql`(${prevResult}) AND (${nextCond})`;
      }
      combined = result;
    }
    return sql`NOT (${combined})`;
  }

  // Handle column conditions
  const columnConditions: Expression<SqlBool>[] = [];

  // Use the schema passed from above
  const currentTable = schema.tables.find((t: any) => t.name === tableName);

  for (const [column, operators] of Object.entries(condition)) {
    if (column === "AND" || column === "OR" || column === "NOT") continue;

    // Check if this is a relation (unqualified name like "orderItems")
    const isRelation = currentTable?.relations?.some(
      (r: any) => r.name === column,
    );

    // All column references must be qualified (table.column format)
    // Relations are the exception - they use unqualified names
    let resolvedTableName: string;
    let resolvedColumnName: string;

    if (column.includes(".")) {
      // Qualified column reference
      const lastDot = column.lastIndexOf(".");
      const qualifiedTable = column.slice(0, lastDot);
      const qualifiedColumn = column.slice(lastDot + 1);

      // Verify table exists in schema
      const targetTable = schema.tables.find((t: any) => t.name === qualifiedTable);
      if (!targetTable) {
        throw new Error(`Table "${qualifiedTable}" not found in schema for column "${column}"`);
      }

      resolvedTableName = qualifiedTable;
      resolvedColumnName = qualifiedColumn;
    } else if (isRelation) {
      // Relations use unqualified names - handled below
      resolvedTableName = tableName;
      resolvedColumnName = column;
    } else if (insideRelationFilter) {
      // Inside relation filters, unqualified columns are allowed
      // They resolve to the current target table
      resolvedTableName = tableName;
      resolvedColumnName = column;
    } else {
      // Unqualified non-relation column at top level
      throw new Error(
        `Column "${column}" must be qualified with a table name. ` +
        `Use "${tableName}.${column}" for the base table, or "joined_table.${column}" if filtering on a joined table.`
      );
    }

    if (operators === null) {
      // Column IS NULL
      columnConditions.push(
        buildOperatorCondition(resolvedColumnName, "equals", null, resolvedTableName, schema, dialect),
      );
    } else if (typeof operators === "object" && !Array.isArray(operators)) {
      if (isRelation) {
        // This is a relation filter (relations are always unqualified)
        const relationCondition = buildRelationFilter(
          column,
          operators,
          tableName,
          schema,
          dialect,
          tableConditions,
        );
        if (relationCondition) {
          columnConditions.push(relationCondition);
        }
      } else {
        // Column operators (column is qualified)
        for (const [operator, value] of Object.entries(operators)) {
          columnConditions.push(
            buildOperatorCondition(
              resolvedColumnName,
              operator,
              value,
              resolvedTableName,
              schema,
              dialect,
            ),
          );
        }
      }
    } else {
      // Direct value comparison (implicit equals)
      columnConditions.push(
        buildOperatorCondition(resolvedColumnName, "equals", operators, resolvedTableName, schema, dialect),
      );
    }
  }

  if (columnConditions.length === 0) return undefined;
  if (columnConditions.length === 1) return columnConditions[0]!;

  let result = columnConditions[0]!;
  for (let i = 1; i < columnConditions.length; i++) {
    const prevResult = result;
    const nextCond = columnConditions[i]!;
    result = sql`(${prevResult}) AND (${nextCond})`;
  }
  return result;
}

function buildRelationFilter(
  relationName: string,
  filterObj: Record<string, any>,
  currentTableName: string,
  schema: SchemaResponse,
  dialect: DatabaseDialect,
  tableConditions?: TableConditionsMap,
): Expression<SqlBool> | undefined {
  // Use the schema passed from above
  const currentTable = schema.tables.find((t) => t.name === currentTableName);

  if (!currentTable) {
    return undefined;
  }

  // Find the relation by name
  const relation = currentTable.relations?.find(
    (r: any) => r.name === relationName,
  );

  if (!relation) {
    // If not found as a relation, it might be a column with relation operators
    // Fall back to treating it as a column (backward compatibility)
    return undefined;
  }

  // Extract relation details
  const targetTable = relation.targetTable;
  const targetSchema = relation.targetSchema;
  const sourceColumns = relation.sourceColumns || [];
  const targetColumns = relation.targetColumns || [];

  if (sourceColumns.length === 0 || targetColumns.length === 0) {
    return undefined;
  }

  // Get the operator and nested condition
  const [[operator, nestedCondition]] = Object.entries(filterObj) as [
    [string, any],
  ];

  // Build schema-qualified table identifiers for cross-schema support
  const currentTableId = getTableIdentifier(currentTableName, currentTable.schema, dialect);
  const targetTableId = getTableIdentifier(targetTable, targetSchema, dialect);
  const targetTableRef = sql.table(targetTableId);

  // Build table condition expression for the target table (row-level security)
  // Use schema-qualified table name for cross-schema relations
  let tableConditionExpr: Expression<SqlBool> | undefined;
  const targetCondition = tableConditions?.[targetTable];
  if (targetCondition) {
    const { column, value } = targetCondition;
    tableConditionExpr = sql<SqlBool>`${sql.ref(`${targetTableId}.${column}`)} = ${value}`;
  }

  // Build the join condition based on whether it's a list relation or not
  let joinCondition: Expression<SqlBool>;

  if (relation.isList) {
    // For list relations (one-to-many): target.targetColumn = current.sourceColumn
    // e.g., orders.user_id = users.id
    // Use schema-qualified table names for cross-schema relations
    const conditions: Expression<SqlBool>[] = sourceColumns.map(
      (sourceCol: string, i: number) =>
        sql<SqlBool>`${sql.ref(
          `${targetTableId}.${targetColumns[i]!}`,
        )} = ${sql.ref(`${currentTableId}.${sourceCol}`)}`,
    );
    if (conditions.length === 1) {
      joinCondition = conditions[0]!;
    } else {
      let result = conditions[0]!;
      for (let i = 1; i < conditions.length; i++) {
        const prevResult = result;
        const nextCond = conditions[i]!;
        result = sql`(${prevResult}) AND (${nextCond})`;
      }
      joinCondition = result;
    }
  } else {
    // For single relations (many-to-one): current.sourceColumn = target.targetColumn
    // e.g., orders.user_id = users.id
    // Use schema-qualified table names for cross-schema relations
    const conditions: Expression<SqlBool>[] = sourceColumns.map(
      (sourceCol: string, i: number) =>
        sql<SqlBool>`${sql.ref(`${currentTableId}.${sourceCol}`)} = ${sql.ref(
          `${targetTableId}.${targetColumns[i]!}`,
        )}`,
    );
    if (conditions.length === 1) {
      joinCondition = conditions[0]!;
    } else {
      let result = conditions[0]!;
      for (let i = 1; i < conditions.length; i++) {
        const prevResult = result;
        const nextCond = conditions[i]!;
        result = sql`(${prevResult}) AND (${nextCond})`;
      }
      joinCondition = result;
    }
  }

  // Helper to build combined WHERE clause for subqueries
  const buildSubqueryWhere = (
    ...exprs: (Expression<SqlBool> | undefined)[]
  ): Expression<SqlBool> => {
    const validExprs = exprs.filter(
      (e): e is Expression<SqlBool> => e !== undefined,
    );
    return combineSqlExpressions(validExprs, "AND");
  };

  // Check if we need to parse nested conditions
  const needsNestedParsing =
    nestedCondition &&
    typeof nestedCondition === "object" &&
    Object.keys(nestedCondition).length > 0;

  if (needsNestedParsing) {
    // Parse nested conditions (pass tableConditions for nested relation filters)
    // insideRelationFilter=true allows unqualified column names relative to target table
    const nestedWhere = parseConditionObject(
      nestedCondition,
      targetTable,
      schema,
      dialect,
      tableConditions,
      true, // insideRelationFilter
    );

    // Build the EXISTS/NOT EXISTS subquery
    switch (operator) {
      case "some":
        // At least one related record matches the condition
        return sql`EXISTS (SELECT 1 FROM ${targetTableRef} WHERE ${buildSubqueryWhere(joinCondition, tableConditionExpr, nestedWhere)})`;

      case "none":
        // No related records match the condition
        return sql`NOT EXISTS (SELECT 1 FROM ${targetTableRef} WHERE ${buildSubqueryWhere(joinCondition, tableConditionExpr, nestedWhere)})`;

      case "every":
        // All related records match the condition (no record violates the condition)
        if (nestedWhere) {
          // "Every child satisfies condition" = "No child violates condition"
          // Note: tableConditionExpr scopes which records we check, nestedWhere is the condition to satisfy
          const baseWhere = buildSubqueryWhere(
            joinCondition,
            tableConditionExpr,
          );
          return sql`NOT EXISTS (SELECT 1 FROM ${targetTableRef} WHERE ${baseWhere} AND NOT (${nestedWhere}))`;
        } else {
          // If no condition, this is always true if there are no related records
          return sql`NOT EXISTS (SELECT 1 FROM ${targetTableRef} WHERE ${buildSubqueryWhere(joinCondition, tableConditionExpr)} AND FALSE)`;
        }

      case "is":
        // To-one relation check (only valid for many-to-one relations)
        if (relation.isList) {
          // 'is' operator doesn't make sense for list relations
          return undefined;
        }

        if (nestedWhere) {
          return sql`EXISTS (SELECT 1 FROM ${targetTableRef} WHERE ${buildSubqueryWhere(joinCondition, tableConditionExpr, nestedWhere)})`;
        } else {
          // No valid conditions, just check if foreign key is not null
          const notNullChecks = sourceColumns.map(
            (col: string) =>
              sql<SqlBool>`${sql.ref(`${currentTableId}.${col}`)} IS NOT NULL`,
          );
          return combineSqlExpressions(notNullChecks, "AND");
        }

      case "isNot":
        // To-one relation check (negated) - only valid for many-to-one relations
        if (relation.isList) {
          // 'isNot' operator doesn't make sense for list relations
          return undefined;
        }

        if (nestedWhere) {
          return sql`NOT EXISTS (SELECT 1 FROM ${targetTableRef} WHERE ${buildSubqueryWhere(joinCondition, tableConditionExpr, nestedWhere)})`;
        } else {
          // Check if foreign key is null
          const nullChecks = sourceColumns.map(
            (col: string) =>
              sql<SqlBool>`${sql.ref(`${currentTableId}.${col}`)} IS NULL`,
          );
          return nullChecks.length === 1
            ? nullChecks[0]
            : sql`(${combineSqlExpressions(nullChecks, "OR")})`;
        }

      default:
        return undefined;
    }
  }

  // Handle cases without nested conditions
  switch (operator) {
    case "some":
      // At least one related record exists
      return sql`EXISTS (SELECT 1 FROM ${targetTableRef} WHERE ${buildSubqueryWhere(joinCondition, tableConditionExpr)})`;

    case "none":
      // No related records exist
      return sql`NOT EXISTS (SELECT 1 FROM ${targetTableRef} WHERE ${buildSubqueryWhere(joinCondition, tableConditionExpr)})`;

    case "every":
      // All related records match (when no condition, this means no records violate it)
      return sql`NOT EXISTS (SELECT 1 FROM ${targetTableRef} WHERE ${buildSubqueryWhere(joinCondition, tableConditionExpr)} AND FALSE)`;

    case "is":
      // To-one relation check
      if (relation.isList) {
        return undefined;
      }

      if (nestedCondition === null) {
        // Check if foreign key is null
        const nullChecks = sourceColumns.map(
          (col: string) =>
            sql<SqlBool>`${sql.ref(`${currentTableId}.${col}`)} IS NULL`,
        );
        return nullChecks.length === 1
          ? nullChecks[0]
          : sql`(${combineSqlExpressions(nullChecks, "OR")})`;
      } else if (Object.keys(nestedCondition).length === 0) {
        // Empty object - check if foreign key is not null
        const notNullChecks = sourceColumns.map(
          (col: string) =>
            sql<SqlBool>`${sql.ref(`${currentTableId}.${col}`)} IS NOT NULL`,
        );
        return combineSqlExpressions(notNullChecks, "AND");
      } else {
        // Has conditions - parse them
        const nestedWhere = parseConditionObject(
          nestedCondition,
          targetTable,
          schema,
          dialect,
          tableConditions,
        );
        if (nestedWhere) {
          return sql`EXISTS (SELECT 1 FROM ${targetTableRef} WHERE ${buildSubqueryWhere(joinCondition, tableConditionExpr, nestedWhere)})`;
        } else {
          // No valid conditions, just check if foreign key is not null
          const notNullChecks = sourceColumns.map(
            (col: string) =>
              sql<SqlBool>`${sql.ref(`${currentTableId}.${col}`)} IS NOT NULL`,
          );
          return combineSqlExpressions(notNullChecks, "AND");
        }
      }

    case "isNot":
      // To-one relation check (negated)
      if (relation.isList) {
        return undefined;
      }

      if (nestedCondition === null) {
        // Check if foreign key is not null
        const notNullChecks = sourceColumns.map(
          (col: string) =>
            sql<SqlBool>`${sql.ref(`${currentTableId}.${col}`)} IS NOT NULL`,
        );
        return combineSqlExpressions(notNullChecks, "AND");
      } else {
        // Parse nested conditions
        const nestedWhere = parseConditionObject(
          nestedCondition,
          targetTable,
          schema,
          dialect,
          tableConditions,
        );
        if (nestedWhere) {
          return sql`NOT EXISTS (SELECT 1 FROM ${targetTableRef} WHERE ${buildSubqueryWhere(joinCondition, tableConditionExpr, nestedWhere)})`;
        } else {
          // Check if foreign key is null
          const nullChecks = sourceColumns.map(
            (col: string) =>
              sql<SqlBool>`${sql.ref(`${currentTableId}.${col}`)} IS NULL`,
          );
          return nullChecks.length === 1
            ? nullChecks[0]
            : sql`(${combineSqlExpressions(nullChecks, "OR")})`;
        }
      }

    default:
      return undefined;
  }
}

function buildOperatorCondition(
  column: string,
  operator: string,
  value: any,
  tableName: string,
  schema: SchemaResponse,
  dialect: DatabaseDialect,
): Expression<SqlBool> {
  const columnRef = getColumnFromTable({
    columnName: column,
    tableName,
    schema,
    dialect,
  });

  // Handle date strings - parse them properly for SQL
  // All supported dialects handle ISO date strings correctly
  const processedValue = value;

  switch (operator) {
    case "equals":
      if (processedValue === null) {
        return sql<SqlBool>`${columnRef} IS NULL`;
      }
      return sql<SqlBool>`${columnRef} = ${processedValue}`;

    case "not":
      if (processedValue === null) {
        return sql<SqlBool>`${columnRef} IS NOT NULL`;
      }
      return sql<SqlBool>`${columnRef} != ${processedValue}`;

    case "in":
      if (!Array.isArray(processedValue) || processedValue.length === 0) {
        return sql`FALSE`;
      }
      return sql<SqlBool>`${columnRef} IN (${sql.join(
        processedValue.map((v) => sql`${v}`),
        sql`, `,
      )})`;

    case "notIn":
      if (!Array.isArray(processedValue) || processedValue.length === 0) {
        return sql`TRUE`;
      }
      return sql<SqlBool>`${columnRef} NOT IN (${sql.join(
        processedValue.map((v) => sql`${v}`),
        sql`, `,
      )})`;

    case "lt":
      return sql<SqlBool>`${columnRef} < ${processedValue}`;

    case "lte":
      return sql<SqlBool>`${columnRef} <= ${processedValue}`;

    case "gt":
      return sql<SqlBool>`${columnRef} > ${processedValue}`;

    case "gte":
      return sql<SqlBool>`${columnRef} >= ${processedValue}`;

    case "contains":
      return sql<SqlBool>`${columnRef} LIKE ${"%" + processedValue + "%"}`;

    case "contains_insensitive":
      return sql<SqlBool>`LOWER(${columnRef}) LIKE LOWER(${
        "%" + processedValue + "%"
      })`;

    case "startsWith":
      return sql<SqlBool>`${columnRef} LIKE ${processedValue + "%"}`;

    case "endsWith":
      return sql<SqlBool>`${columnRef} LIKE ${"%" + processedValue}`;

    default:
      // Fallback to equals
      return sql`${columnRef} = ${processedValue}`;
  }
}

function isDateString(value: string): boolean {
  // Check if the string matches common date formats
  // ISO 8601 format: YYYY-MM-DDTHH:mm:ss.sssZ
  const isoDateRegex = /^\d{4}-\d{2}-\d{2}(T\d{2}:\d{2}:\d{2}(\.\d{3})?Z?)?$/;
  if (isoDateRegex.test(value)) {
    const date = new Date(value);
    return !isNaN(date.getTime());
  }
  return false;
}
