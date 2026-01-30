import type { WhereAndArray } from "@repo/types";

export type Operation =
  | "findMany"
  | "findDistinct"
  | "findDistinctByEditDistance"
  | "count"
  | "countRelations"
  | "aggregate"
  | "aggregateGroups"
  | "groupBy";

export interface DBQuery {
  table: string;
  tableSchema?: string | null;  // Database schema name (e.g., 'public', 'sales')
  operation: string;
  operationParameters: Record<string, unknown>;
  whereAndArray?: WhereAndArray;
  tableConditions: Record<
    string,
    { column: string; value: string | number }
  > | null;
}
