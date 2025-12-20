import type { TableConditions, QuestionConditions } from "@repo/types";
import type { DateCondition } from "@repo/types";

export function generatePrismaWhereArray(
  tableCondition: TableConditions,
  questionCondition: QuestionConditions,
  dateCondition: DateCondition,
) {
  const whereAndArray = [];
  if (tableCondition && tableCondition.length > 0) {
    const whereConditions = tableCondition.reduce(
      (
        acc: Record<
          string,
          Record<string, string | number | null | object | boolean>
        >,
        tableCondition,
      ) => {
        acc[tableCondition.column] = {
          [tableCondition.operator]: tableCondition.value,
        };
        return acc;
      },
      {},
    );
    whereAndArray.push(whereConditions);
  }
  if (questionCondition) {
    whereAndArray.push(questionCondition);
  }
  if (dateCondition) {
    const dateConditions = dateCondition.OR.map((dateRange) => {
      const dateLimits = dateRange.AND.map((dateLimit) => {
        return {
          [dateLimit.column]: {
            [dateLimit.operator]: dateLimit.value,
          },
        };
      });
      return {
        AND: [...dateLimits],
      };
    });
    whereAndArray.push({ OR: [...dateConditions] });
  }
  return whereAndArray;
}
