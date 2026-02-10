import { type SQLComputedColumnAst } from "@repo/types";

/**
 * Converts a computed column AST to a readable string representation
 */
export function computedColumnToString(ast: SQLComputedColumnAst): string {
  switch (ast.type) {
    case "column":
      return ast.name;
    case "value":
      return ast.value.toString();
    case "operation": {
      const [left, right] = ast.operands;
      if (!left || !right) {
        return "Invalid operation";
      }
      return `${computedColumnToString(left)} ${
        ast.operator
      } ${computedColumnToString(right)}`;
    }
    case "brackets":
      return `(${computedColumnToString(ast.expression)})`;
    default:
      return "Invalid expression";
  }
}
