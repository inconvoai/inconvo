import type { QuestionConditions } from "@repo/types";
import type { ContextCondition } from "../index";

export function generatePrismaWhereArray(
  contextConditions: ContextCondition[],
  questionConditions: QuestionConditions,
) {
  const whereAndArray = [];

  // Add context conditions with qualified column names (table.column)
  if (contextConditions && contextConditions.length > 0) {
    const whereConditions = contextConditions.reduce(
      (
        acc: Record<
          string,
          Record<string, string | number | null | object | boolean>
        >,
        condition,
      ) => {
        const qualifiedColumn = `${condition.table}.${condition.column}`;
        acc[qualifiedColumn] = {
          [condition.operator]: condition.value,
        };
        return acc;
      },
      {},
    );
    whereAndArray.push(whereConditions);
  }

  // Add question-derived conditions
  if (questionConditions) {
    whereAndArray.push(questionConditions);
  }

  return whereAndArray;
}
