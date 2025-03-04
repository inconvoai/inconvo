import { ComputedColumn, WhereConditions } from "~/types/querySchema";

export async function loadDrizzleTables() {
  try {
    const modulePath = `../../drizzle/schema.js`;
    const tablesModule = await import(modulePath);
    return tablesModule as Record<string, any>;
  } catch (error) {
    console.error("Failed to load Drizzle tables:", error);
    throw error;
  }
}

export function getSelectColumns(
  cols: string[],
  computedCols?: { name: string }[]
): Record<string, boolean> {
  return [
    ...cols,
    ...(computedCols ? computedCols.map((column) => column.name) : []),
  ].reduce((acc: Record<string, boolean>, column) => {
    acc[column] = true;
    return acc;
  }, {});
}

export function filterResponseWithComputedConditions(
  response: any[],
  computedWhere: any[]
): any[] {
  return response.filter((row: { [x: string]: any }) => {
    return computedWhere.every((condition) => {
      const [columnName] = Object.keys(condition);
      const columnCondition = condition[columnName];
      if (columnCondition.lte !== undefined) {
        return row[columnName] <= columnCondition.lte;
      }
      if (columnCondition.gte !== undefined) {
        return row[columnName] >= columnCondition.gte;
      }
      if (columnCondition.not !== undefined) {
        return row[columnName] !== columnCondition.not;
      }
      if (columnCondition.equals !== undefined) {
        return row[columnName] === columnCondition.equals;
      }
      if (columnCondition.lt !== undefined) {
        return row[columnName] < columnCondition.lt;
      }
      if (columnCondition.gt !== undefined) {
        return row[columnName] > columnCondition.gt;
      }
      return true;
    });
  });
}

export function splitWhereConditions(
  computedColumns: ComputedColumn[],
  whereAndArray: WhereConditions
): [computedWhere: WhereConditions, dbWhere: WhereConditions] {
  const computedWhere: WhereConditions = [];
  const dbWhere: WhereConditions = [];

  if (whereAndArray?.length === 0) {
    return [computedWhere, dbWhere];
  }

  whereAndArray.forEach((condition) => {
    const whereColumnNames = Object.keys(condition);
    const isComputed = whereColumnNames.some((columnName) =>
      computedColumns?.some((column) => column.name === columnName)
    );
    if (isComputed) {
      computedWhere.push(condition);
    } else {
      dbWhere.push(condition);
    }
  });

  return [computedWhere, dbWhere];
}
