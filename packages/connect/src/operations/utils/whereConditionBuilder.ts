import { sql, Expression, SqlBool } from "kysely";
import { WhereConditions } from "~/types/querySchema";
import { getColumnFromTable } from "./computedColumns";
import { env } from "~/env";
import type { SchemaResponse } from "~/types/types";

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
    return expressions[0];
  }

  let result = expressions[0];
  for (let i = 1; i < expressions.length; i++) {
    const prevResult = result;
    const nextExpr = expressions[i];
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
): Expression<SqlBool> | undefined {
  if (!whereAndArray || whereAndArray.length === 0) {
    return undefined;
  }

  // Process the whereAndArray as an implicit AND of all conditions
  const topLevelConditions: Expression<SqlBool>[] = [];

  for (let i = 0; i < whereAndArray.length; i++) {
    const whereGroup = whereAndArray[i];
    if (whereGroup === null || whereGroup === undefined) continue;

    const condition = parseConditionObject(whereGroup, tableName, schema);
    if (condition) {
      topLevelConditions.push(condition);
    }
  }

  if (topLevelConditions.length === 0) {
    return undefined;
  }

  if (topLevelConditions.length === 1) {
    return topLevelConditions[0];
  }

  // Build AND expression without using reduce to avoid RawBuilder await issues
  let result = topLevelConditions[0];
  for (let i = 1; i < topLevelConditions.length; i++) {
    const prevResult = result;
    const nextCond = topLevelConditions[i];
    result = sql`(${prevResult}) AND (${nextCond})`;
  }
  return result;
}

function parseConditionObject(
  condition: any,
  tableName: string,
  schema: SchemaResponse,
): Expression<SqlBool> | undefined {
  if (!condition || typeof condition !== "object") {
    return undefined;
  }

  // Handle AND
  if ("AND" in condition && Array.isArray(condition.AND)) {
    const andConditions: Expression<SqlBool>[] = [];

    for (const subCondition of condition.AND) {
      const parsed = parseConditionObject(subCondition, tableName, schema);
      if (parsed !== undefined) {
        andConditions.push(parsed);
      }
    }

    if (andConditions.length === 0) return undefined;
    if (andConditions.length === 1) return andConditions[0];

    let result = andConditions[0];
    for (let i = 1; i < andConditions.length; i++) {
      const prevResult = result;
      const nextCond = andConditions[i];
      result = sql`(${prevResult}) AND (${nextCond})`;
    }
    return result;
  }

  // Handle OR
  if ("OR" in condition && Array.isArray(condition.OR)) {
    const orConditions: Expression<SqlBool>[] = [];

    for (const subCondition of condition.OR) {
      const parsed = parseConditionObject(subCondition, tableName, schema);
      if (parsed !== undefined) {
        orConditions.push(parsed);
      }
    }

    if (orConditions.length === 0) return undefined;
    if (orConditions.length === 1) return orConditions[0];

    let result = orConditions[0];
    for (let i = 1; i < orConditions.length; i++) {
      const prevResult = result;
      const nextCond = orConditions[i];
      result = sql`(${prevResult}) OR (${nextCond})`;
    }
    return sql`(${result})`;
  }

  // Handle NOT
  if ("NOT" in condition && Array.isArray(condition.NOT)) {
    const notConditions: Expression<SqlBool>[] = [];

    for (const subCondition of condition.NOT) {
      const parsed = parseConditionObject(subCondition, tableName, schema);
      if (parsed !== undefined) {
        notConditions.push(parsed);
      }
    }

    if (notConditions.length === 0) return undefined;

    let combined: Expression<SqlBool>;
    if (notConditions.length === 1) {
      combined = notConditions[0];
    } else {
      let result = notConditions[0];
      for (let i = 1; i < notConditions.length; i++) {
        const prevResult = result;
        const nextCond = notConditions[i];
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

    // Check if this column is a relation
    const isRelation = currentTable?.relations?.some(
      (r: any) => r.name === column,
    );

    if (operators === null) {
      // Column IS NULL
      columnConditions.push(
        buildOperatorCondition(column, "equals", null, tableName, schema),
      );
    } else if (typeof operators === "object" && !Array.isArray(operators)) {
      // Check if this is a relation filter
      if (isRelation) {
        // This is a relation filter
        const relationCondition = buildRelationFilter(
          column,
          operators,
          tableName,
          schema,
        );
        if (relationCondition) {
          columnConditions.push(relationCondition);
        }
      } else {
        // Multiple operators for the column
        for (const [operator, value] of Object.entries(operators)) {
          // Handle relation operators
          if (
            operator === "is" ||
            operator === "isNot" ||
            operator === "some" ||
            operator === "every" ||
            operator === "none"
          ) {
            const relationCondition = buildRelationFilter(
              column,
              { [operator]: value },
              tableName,
              schema,
            );
            if (relationCondition) {
              columnConditions.push(relationCondition);
            }
          } else {
            columnConditions.push(
              buildOperatorCondition(
                column,
                operator,
                value,
                tableName,
                schema,
              ),
            );
          }
        }
      }
    } else {
      // Direct value comparison (implicit equals)
      columnConditions.push(
        buildOperatorCondition(column, "equals", operators, tableName, schema),
      );
    }
  }

  if (columnConditions.length === 0) return undefined;
  if (columnConditions.length === 1) return columnConditions[0];

  let result = columnConditions[0];
  for (let i = 1; i < columnConditions.length; i++) {
    const prevResult = result;
    const nextCond = columnConditions[i];
    result = sql`(${prevResult}) AND (${nextCond})`;
  }
  return result;
}

function buildRelationFilter(
  relationName: string,
  filterObj: Record<string, any>,
  currentTableName: string,
  schema: SchemaResponse,
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
  const sourceColumns = relation.sourceColumns || [];
  const targetColumns = relation.targetColumns || [];

  if (sourceColumns.length === 0 || targetColumns.length === 0) {
    return undefined;
  }

  // Get the operator and nested condition
  const [[operator, nestedCondition]] = Object.entries(filterObj);

  // Build the subquery
  const targetTableRef = sql.table(targetTable);

  // Build the join condition based on whether it's a list relation or not
  let joinCondition: Expression<SqlBool>;

  if (relation.isList) {
    // For list relations (one-to-many): target.targetColumn = current.sourceColumn
    // e.g., orders.user_id = users.id
    const conditions: Expression<SqlBool>[] = sourceColumns.map(
      (sourceCol: string, i: number) =>
        sql<SqlBool>`${sql.ref(
          `${targetTable}.${targetColumns[i]}`,
        )} = ${sql.ref(`${currentTableName}.${sourceCol}`)}`,
    );
    if (conditions.length === 1) {
      joinCondition = conditions[0];
    } else {
      let result = conditions[0];
      for (let i = 1; i < conditions.length; i++) {
        const prevResult = result;
        const nextCond = conditions[i];
        result = sql`(${prevResult}) AND (${nextCond})`;
      }
      joinCondition = result;
    }
  } else {
    // For single relations (many-to-one): current.sourceColumn = target.targetColumn
    // e.g., orders.user_id = users.id
    const conditions: Expression<SqlBool>[] = sourceColumns.map(
      (sourceCol: string, i: number) =>
        sql<SqlBool>`${sql.ref(`${currentTableName}.${sourceCol}`)} = ${sql.ref(
          `${targetTable}.${targetColumns[i]}`,
        )}`,
    );
    if (conditions.length === 1) {
      joinCondition = conditions[0];
    } else {
      let result = conditions[0];
      for (let i = 1; i < conditions.length; i++) {
        const prevResult = result;
        const nextCond = conditions[i];
        result = sql`(${prevResult}) AND (${nextCond})`;
      }
      joinCondition = result;
    }
  }

  // Check if we need to parse nested conditions
  const needsNestedParsing =
    nestedCondition &&
    typeof nestedCondition === "object" &&
    Object.keys(nestedCondition).length > 0;

  if (needsNestedParsing) {
    // Parse nested conditions
    const nestedWhere = parseConditionObject(
      nestedCondition,
      targetTable,
      schema,
    );

    // Build the EXISTS/NOT EXISTS subquery
    switch (operator) {
      case "some":
        // At least one related record matches the condition
        if (nestedWhere) {
          return sql`EXISTS (SELECT 1 FROM ${targetTableRef} WHERE ${joinCondition} AND ${nestedWhere})`;
        } else {
          return sql`EXISTS (SELECT 1 FROM ${targetTableRef} WHERE ${joinCondition})`;
        }

      case "none":
        // No related records match the condition
        if (nestedWhere) {
          return sql`NOT EXISTS (SELECT 1 FROM ${targetTableRef} WHERE ${joinCondition} AND ${nestedWhere})`;
        } else {
          return sql`NOT EXISTS (SELECT 1 FROM ${targetTableRef} WHERE ${joinCondition})`;
        }

      case "every":
        // All related records match the condition (no record violates the condition)
        if (nestedWhere) {
          // "Every child satisfies condition" = "No child violates condition"
          return sql`NOT EXISTS (SELECT 1 FROM ${targetTableRef} WHERE ${joinCondition} AND NOT (${nestedWhere}))`;
        } else {
          // If no condition, this is always true if there are no related records
          return sql`NOT EXISTS (SELECT 1 FROM ${targetTableRef} WHERE ${joinCondition} AND FALSE)`;
        }

      case "is":
        // To-one relation check (only valid for many-to-one relations)
        if (relation.isList) {
          // 'is' operator doesn't make sense for list relations
          return undefined;
        }

        if (nestedWhere) {
          return sql`EXISTS (SELECT 1 FROM ${targetTableRef} WHERE ${joinCondition} AND ${nestedWhere})`;
        } else {
          // No valid conditions, just check if foreign key is not null
          const notNullChecks = sourceColumns.map(
            (col: string) =>
              sql<SqlBool>`${sql.ref(`${currentTableName}.${col}`)} IS NOT NULL`,
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
          return sql`NOT EXISTS (SELECT 1 FROM ${targetTableRef} WHERE ${joinCondition} AND ${nestedWhere})`;
        } else {
          // Check if foreign key is null
          const nullChecks = sourceColumns.map(
            (col: string) =>
              sql<SqlBool>`${sql.ref(`${currentTableName}.${col}`)} IS NULL`,
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
      return sql`EXISTS (SELECT 1 FROM ${targetTableRef} WHERE ${joinCondition})`;

    case "none":
      // No related records exist
      return sql`NOT EXISTS (SELECT 1 FROM ${targetTableRef} WHERE ${joinCondition})`;

    case "every":
      // All related records match (when no condition, this means no records violate it)
      return sql`NOT EXISTS (SELECT 1 FROM ${targetTableRef} WHERE ${joinCondition} AND FALSE)`;

    case "is":
      // To-one relation check
      if (relation.isList) {
        return undefined;
      }

      if (nestedCondition === null) {
        // Check if foreign key is null
        const nullChecks = sourceColumns.map(
          (col: string) =>
            sql<SqlBool>`${sql.ref(`${currentTableName}.${col}`)} IS NULL`,
        );
        return nullChecks.length === 1
          ? nullChecks[0]
          : sql`(${combineSqlExpressions(nullChecks, "OR")})`;
      } else if (Object.keys(nestedCondition).length === 0) {
        // Empty object - check if foreign key is not null
        const notNullChecks = sourceColumns.map(
          (col: string) =>
            sql<SqlBool>`${sql.ref(`${currentTableName}.${col}`)} IS NOT NULL`,
        );
        return combineSqlExpressions(notNullChecks, "AND");
      } else {
        // Has conditions - parse them
        const nestedWhere = parseConditionObject(
          nestedCondition,
          targetTable,
          schema,
        );
        if (nestedWhere) {
          return sql`EXISTS (SELECT 1 FROM ${targetTableRef} WHERE ${joinCondition} AND ${nestedWhere})`;
        } else {
          // No valid conditions, just check if foreign key is not null
          const notNullChecks = sourceColumns.map(
            (col: string) =>
              sql<SqlBool>`${sql.ref(`${currentTableName}.${col}`)} IS NOT NULL`,
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
            sql<SqlBool>`${sql.ref(`${currentTableName}.${col}`)} IS NOT NULL`,
        );
        return combineSqlExpressions(notNullChecks, "AND");
      } else {
        // Parse nested conditions
        const nestedWhere = parseConditionObject(
          nestedCondition,
          targetTable,
          schema,
        );
        if (nestedWhere) {
          return sql`NOT EXISTS (SELECT 1 FROM ${targetTableRef} WHERE ${joinCondition} AND ${nestedWhere})`;
        } else {
          // Check if foreign key is null
          const nullChecks = sourceColumns.map(
            (col: string) =>
              sql<SqlBool>`${sql.ref(`${currentTableName}.${col}`)} IS NULL`,
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
): Expression<SqlBool> {
  const columnRef = getColumnFromTable({
    columnName: column,
    tableName,
    schema,
  });

  // Handle date strings - parse them properly for SQL
  let processedValue = value;
  if (typeof value === "string" && isDateString(value)) {
    // For date strings, we need to handle them based on the database dialect
    if (env.DATABASE_DIALECT === "mssql") {
      // MSSQL can handle ISO date strings directly
      processedValue = value;
    } else {
      // PostgreSQL and MySQL also handle ISO strings well
      processedValue = value;
    }
  }

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
