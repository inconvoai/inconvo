import type { WhereAndArray } from "~/server/userDatabaseConnector/types";

export type Operation =
  | "findMany"
  | "findDistinct"
  | "findDistinctByEditDistance"
  | "count"
  | "countRelations"
  | "aggregate"
  | "groupBy"
  | "groupByDateInterval"
  | "countByTemporalComponent";

export interface DBQuery {
  table: string;
  operation: string;
  operationParameters: Record<string, unknown>;
  whereAndArray?: WhereAndArray;
  computedColumns?: Record<string, unknown>[];
  jsonColumnSchema?: unknown;
}
