import { JoinBuilder } from "kysely";
import type { JoinPathHop } from "../../types/querySchema";
import type { SchemaResponse, DatabaseDialect } from "../../types/types";
import { getTableIdentifier } from "./tableIdentifier";

export type QualifiedColumn = {
  tableName: string;
  columnName: string;
};

export function parseQualifiedColumn(column: string): QualifiedColumn {
  const parts = column.split(".");
  if (parts.length !== 2) {
    throw new Error(
      `Join path columns must be fully qualified (table.column). Received ${column}`,
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
      `Join path hop must pair equal numbers of source and target columns.`,
    );
  }
  return {
    source: hop.source.map(parseQualifiedColumn),
    target: hop.target.map(parseQualifiedColumn),
  };
}

function applyJoinConditions(
  joinBuilder: JoinBuilder<any, any>,
  sourceRefs: string[],
  targetRefs: string[],
) {
  let builder = joinBuilder.onRef(sourceRefs[0]!, "=", targetRefs[0]!);
  for (let index = 1; index < sourceRefs.length; index++) {
    builder = builder.onRef(sourceRefs[index]!, "=", targetRefs[index]!);
  }
  return builder;
}

export function applyJoinHop(
  query: any,
  joinType: "inner" | "left" | "right",
  hop: JoinHopMetadata,
  schema?: SchemaResponse,
  dialect?: DatabaseDialect,
) {
  const targetTableName = hop.target[0]?.tableName;
  if (!targetTableName) {
    throw new Error("Join hop is missing target table metadata.");
  }

  // Look up the target table's schema for cross-schema joins
  const targetTable = schema?.tables.find((t) => t.name === targetTableName);
  const targetTableId = schema && dialect
    ? getTableIdentifier(targetTableName, targetTable?.schema, dialect)
    : targetTableName;

  // Build column refs with schema-qualified table names
  const getTableId = (tableName: string) => {
    if (!schema || !dialect) return tableName;
    if (dialect === "bigquery") return tableName;
    const table = schema.tables.find((t) => t.name === tableName);
    return getTableIdentifier(tableName, table?.schema, dialect);
  };

  const sourceRefs = hop.source.map(
    (column) => `${getTableId(column.tableName)}.${column.columnName}`,
  );
  const targetRefs = hop.target.map(
    (column) => `${getTableId(column.tableName)}.${column.columnName}`,
  );

  switch (joinType) {
    case "inner":
      return query.innerJoin(targetTableId, (jb: JoinBuilder<any, any>) =>
        applyJoinConditions(jb, sourceRefs, targetRefs),
      );
    case "right":
      return query.rightJoin(targetTableId, (jb: JoinBuilder<any, any>) =>
        applyJoinConditions(jb, sourceRefs, targetRefs),
      );
    case "left":
    default:
      return query.leftJoin(targetTableId, (jb: JoinBuilder<any, any>) =>
        applyJoinConditions(jb, sourceRefs, targetRefs),
      );
  }
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
