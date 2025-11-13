import { SQL, and, eq } from "drizzle-orm";
import { getColumnFromTable } from "./getColumnFromTable";
import type { ComputedColumn, JoinPathHop } from "~/types/querySchema";

export type QualifiedColumn = {
  tableName: string;
  columnName: string;
};

export function parseQualifiedColumn(column: string): QualifiedColumn {
  const parts = column.split(".");
  if (parts.length !== 2) {
    throw new Error(
      `Join path columns must be fully qualified (table.column). Received ${column}`
    );
  }
  const [tableName, columnName] = parts;
  if (!tableName || !columnName) {
    throw new Error(`Invalid qualified column: ${column}`);
  }
  return { tableName, columnName };
}

export type JoinHopMetadata = {
  source: QualifiedColumn[];
  target: QualifiedColumn[];
};

export function normaliseJoinHop(hop: JoinPathHop): JoinHopMetadata {
  if (hop.source.length !== hop.target.length) {
    throw new Error(
      `Join path hop must pair equal numbers of source and target columns.`
    );
  }
  return {
    source: hop.source.map(parseQualifiedColumn),
    target: hop.target.map(parseQualifiedColumn),
  };
}

export function buildJoinCondition(
  hop: JoinHopMetadata,
  drizzleSchema: Record<string, any>,
  computedColumns: ComputedColumn[] | undefined
) {
  let joinCondition: SQL<unknown> | undefined;

  hop.source.forEach((sourceColumn, index) => {
    const targetColumn = hop.target[index];

    const left = getColumnFromTable({
      columnName: sourceColumn.columnName,
      tableName: sourceColumn.tableName,
      drizzleSchema,
      computedColumns,
    });

    const right = getColumnFromTable({
      columnName: targetColumn.columnName,
      tableName: targetColumn.tableName,
      drizzleSchema,
      computedColumns,
    });

    const condition = eq(left, right);
    joinCondition = joinCondition ? and(joinCondition, condition) : condition;
  });

  if (!joinCondition) {
    throw new Error("Join hop produced no equality expressions.");
  }

  return joinCondition;
}

export type ResolvedJoinDescriptor = {
  alias: string;
  tableName: string;
  joinType: "inner" | "left" | "right";
  hops: JoinHopMetadata[];
};

export function resolveJoinDescriptor({
  alias,
  tableName,
  joinType,
  path,
}: {
  alias: string;
  tableName: string;
  joinType?: "inner" | "left" | "right";
  path: JoinPathHop[];
}): ResolvedJoinDescriptor {
  return {
    alias,
    tableName,
    joinType: joinType ?? "left",
    hops: path.map(normaliseJoinHop),
  };
}

export function aliasDepth(alias: string) {
  return alias.split(".").length;
}
