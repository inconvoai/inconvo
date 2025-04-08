import { SQL, sql } from "drizzle-orm";
import { ComputedColumn } from "~/types/querySchema";

export function generateComputedColumnAsSQL(
  expression: ComputedColumn["ast"],
  tableName: string,
  drizzleSchema: Record<string, any>
): SQL<number> {
  switch (expression.type) {
    case "column":
      return sql`${drizzleSchema[tableName][expression.name]}`.mapWith(Number);
    case "value":
      return sql.raw(`${expression.value}`).mapWith(Number);
    case "operation": {
      // Recursively convert each operand.
      const operandSQLs = expression.operands.map((operand) =>
        generateComputedColumnAsSQL(operand, tableName, drizzleSchema)
      );
      // Join operands with the operator and wrap in parentheses.
      return sql`(${sql.join(
        operandSQLs,
        sql.raw(` ${expression.operator} `)
      )})`.mapWith(Number);
    }
    case "brackets":
      return sql`(${generateComputedColumnAsSQL(
        expression.expression,
        tableName,
        drizzleSchema
      )})`.mapWith(Number);

    default:
      throw new Error("Unknown SQL expression type");
  }
}
