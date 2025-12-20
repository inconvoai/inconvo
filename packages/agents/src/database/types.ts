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
  operation: string;
  operationParameters: Record<string, unknown>;
  whereAndArray?: WhereAndArray;
  jsonColumnSchema?: unknown;
}
