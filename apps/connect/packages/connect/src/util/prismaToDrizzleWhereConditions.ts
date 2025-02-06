import { eq, ne, gt, gte, lt, lte, and, or, not, SQL, sql } from "drizzle-orm";

function getISOFormatDateQuery(value: string): SQL<string> {
  return sql<string>`to_char(${value}, 'YYYY-MM-DD"T"HH24:MI:SS"Z"')`;
}

/**
 * Parses an individual operator on a column:
 *
 *   { columnName: { gte: "2024-01-01T00:00:00.000Z" } }
 *
 * Be sure to extend this with whichever operators you need (e.g. 'in', etc.)
 */
function parseColumnFilter(
  table: Record<string, any>, // Drizzle table definition
  columnName: string,
  filterObj: Record<string, any> // e.g. { gte: "2024-01-01T00:00:00.000Z" }
) {
  const [operator, value] = Object.entries(filterObj)[0];
  const isDateString = (val: any) => {
    const iso8601Regex = /^\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}\.\d{3}Z$/;
    return typeof val === "string" && iso8601Regex.test(val);
  };

  if (isDateString(value)) {
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
          `Unsupported operator "${operator}" in filter for "${columnName}"`
        );
    }
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

    default:
      throw new Error(
        `Unsupported operator "${operator}" in filter for "${columnName}"`
      );
  }
}

/**
 * Parses a "plain" condition object that is not nested with AND/OR keys.
 * E.g.:
 *
 *   {
 *     create_date_utc: { gte: "2025-02-04T15:50:23.590Z" },
 *     another_col: { equals: 123 }
 *   }
 *
 * -> and( gte(table.create_date_utc, "2025-02-04T15:50:23.590Z"), eq(table.another_col, 123) )
 */
function parseSingleCondition(
  table: Record<string, any>,
  conditionObject: Record<string, any>
) {
  const expressions = Object.entries(conditionObject).map(
    ([columnName, filterObj]) => parseColumnFilter(table, columnName, filterObj)
  );
  // If multiple fields appear in one object, Prisma ANDs them
  return expressions.length > 1 ? and(...expressions) : expressions[0];
}

/**
 * Recursively parses a condition that may contain AND / OR / NOT or a plain object.
 * Also handles array-of-conditions as "AND everything at this level" by default.
 *
 * Examples of supported shapes:
 *
 * - { OR: [ { colA: { equals: 1 } }, { colB: { gt: 99 } } ] }
 * - { AND: [ ... ] }
 * - { NOT: [ { colA: { equals: 2 } } ] }
 * - { colName: { lte: 123 } } (plain object)
 * - [ { OR: [...] }, { col: { gt: 0 } } ] (top-level array => AND each item)
 */
function parseCondition(
  table: Record<string, any>,
  condition: any
): ReturnType<typeof and | typeof or | typeof not> {
  // If it's an array, interpret each element as a sub-condition
  // and combine them with AND (the Prisma default at "top" level).
  if (Array.isArray(condition)) {
    const andClauses = condition.map((c) => parseCondition(table, c));
    return and(...andClauses);
  }

  // If it's an object, check if it has AND / OR / NOT
  if (condition && typeof condition === "object") {
    if (condition.OR) {
      const orClauses = condition.OR.map((c: any) => parseCondition(table, c));
      return or(...orClauses);
    }
    if (condition.AND) {
      const andClauses = condition.AND.map((c: any) =>
        parseCondition(table, c)
      );
      return and(...andClauses);
    }
    if (condition.NOT) {
      // "NOT" can contain multiple clauses, so we AND them together internally,
      // then wrap with not(...)
      const notClauses = condition.NOT.map((c: any) =>
        parseCondition(table, c)
      );
      const combined =
        notClauses.length > 1 ? and(...notClauses) : notClauses[0];
      return not(combined);
    }

    // Otherwise, assume it's a "plain" object with column filters
    return parseSingleCondition(table, condition);
  }

  throw new Error(`Unsupported condition format: ${JSON.stringify(condition)}`);
}

/**
 * Optional helper that you can call with your top-level Prisma-like "where" object or array.
 * If it's an array, it ANDs them together; if it's an object, parse it.
 */
export function parsePrismaWhere(
  table: Record<string, any>,
  where: any
): ReturnType<typeof parseCondition> | undefined {
  if (!where || (Array.isArray(where) && where.length === 0)) {
    return undefined; // or return something else if you prefer
  }
  return parseCondition(table, where);
}
