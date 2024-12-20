import { type PrismaClient } from "@prisma/client";
import { type Query } from "~/types/querySchema";
import { all, create } from "mathjs";

const math = create(all);

function extractNeededColumns(
  ast: any,
  needs: Record<string, boolean> = {}
): Record<string, boolean> {
  if (ast.mathjs === "SymbolNode" && ast.name) {
    needs[ast.name] = true;
  }
  if (ast.args && Array.isArray(ast.args)) {
    ast.args.forEach((arg: any) => extractNeededColumns(arg, needs));
  }
  if (ast.content) {
    extractNeededColumns(ast.content, needs);
  }
  return needs;
}

export function generatePrismaClientWithComputedColumns(
  prisma: PrismaClient,
  table: string,
  computedColumns: Query["computedColumns"]
) {
  if (!computedColumns || computedColumns.length === 0) {
    return prisma;
  }

  const result = computedColumns.reduce((acc, column) => {
    const necessaryCols = extractNeededColumns(column.ast);
    acc[column.name] = {
      needs: necessaryCols,
      compute: (scope: Record<string, any>) => {
        const expr = JSON.parse(JSON.stringify(column.ast), math.reviver);
        return Number(expr.evaluate(scope));
      },
    };
    return acc;
  }, {} as Record<string, any>);

  return prisma.$extends({
    result: {
      [table]: result,
    },
  });
}

export type PrismaComputedColumnClient = ReturnType<
  typeof generatePrismaClientWithComputedColumns
>;
