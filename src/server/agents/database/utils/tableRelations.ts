import assert from "assert";
import type { Schema } from "~/server/db/schema";
import type { JoinPathHop } from "~/server/userDatabaseConnector/types";

export function findRelationsToAColumn(
  schema: Schema,
  columnName: string,
  tableName: string
) {
  const table = schema.find((table) => table.name === tableName);
  assert(table, `Table ${tableName} not found in schema`);
  const columnObject = table.columns.find((col) => col.name === columnName);
  assert(columnObject, `Column ${columnName} not found in table ${tableName}`);

  const relationEntries = Array.isArray(columnObject.relation)
    ? columnObject.relation
    : [];
  if (relationEntries.length === 0) {
    return null;
  }
  const result: Record<string, string[]> = {};

  for (const rel of relationEntries) {
    const targetTableName = rel.relation?.targetTable?.name;
    if (!targetTableName) {
      continue;
    }
    const targetColumns = schema.find(
      (table) => table.name === targetTableName
    )?.columns;

    if (!targetColumns) {
      throw new Error(`Table ${targetTableName} not found in schema`);
    }

    result[targetTableName] = targetColumns.map((col) => col.name);
  }
  return Object.keys(result).length > 0 ? result : null;
}

export interface GeneratedJoinOption {
  name: string;
  table: string;
  path: JoinPathHop[];
  selectableColumns: string[];
}

export interface GeneratedJoinGraph {
  baseAlias: string;
  aliasToTable: Record<string, string>;
  joinOptions: GeneratedJoinOption[];
  uniqueTableNames: string[];
}

type QueueEntry = {
  alias: string;
  table: string;
  path: JoinPathHop[];
  depth: number;
  visitedTables: Set<string>;
};

export function generateJoinGraph(
  schema: Schema,
  startingTableName: string,
  maxDepth: number
): GeneratedJoinGraph {
  const baseTableSchema = schema.find((table) => table.name === startingTableName);
  assert(baseTableSchema, `Table ${startingTableName} not found in schema`);

  const aliasToTable: Record<string, string> = {
    [startingTableName]: startingTableName,
  };
  const joinOptions: GeneratedJoinOption[] = [];
  const uniqueTableNames = new Set<string>([startingTableName]);

  const queue: QueueEntry[] = [];
  const minDepthByTable = new Map<string, number>([[startingTableName, 0]]);

  for (const relation of baseTableSchema.outwardRelations ?? []) {
    const entry = buildQueueEntry(
      startingTableName,
      relation,
      1,
      null,
      new Set([startingTableName])
    );
    if (entry) {
      queue.push(entry);
    }
  }

  const visitedAliases = new Set<string>();

  while (queue.length > 0) {
    const current = queue.shift()!;
    if (visitedAliases.has(current.alias)) continue;
    visitedAliases.add(current.alias);

    const existingDepth = minDepthByTable.get(current.table);
    if (existingDepth !== undefined && existingDepth <= current.depth) {
      continue;
    }
    minDepthByTable.set(current.table, current.depth);

    aliasToTable[current.alias] = current.table;
    uniqueTableNames.add(current.table);

    const tableSchema = schema.find((table) => table.name === current.table);
    if (!tableSchema) continue;

    joinOptions.push({
      name: current.alias,
      table: current.table,
      path: current.path,
      selectableColumns: collectSelectableColumns(tableSchema),
    });

    if (current.depth >= maxDepth) continue;

    for (const relation of tableSchema.outwardRelations ?? []) {
      const entry = buildQueueEntry(
        current.table,
        relation,
        current.depth + 1,
        current,
        current.visitedTables
      );
      if (entry) {
        queue.push(entry);
      }
    }
  }

  return {
    baseAlias: startingTableName,
    aliasToTable,
    joinOptions,
    uniqueTableNames: Array.from(uniqueTableNames),
  };
}

function buildQueueEntry(
  sourceTable: string,
  relation: NonNullable<Schema[number]["outwardRelations"]>[number],
  depth: number,
  parent: QueueEntry | null,
  visited: Set<string>
): QueueEntry | null {
  const targetTableName = relation.targetTable?.name;
  if (!targetTableName) return null;
  if (!relation.sourceColumns?.length || !relation.targetColumns?.length) return null;

  const visitedTables = parent
    ? new Set(parent.visitedTables)
    : new Set(visited);

  if (visitedTables.has(targetTableName)) {
    return null;
  }
  visitedTables.add(targetTableName);

  const relationLabel = relation.name ?? targetTableName;
  const alias = parent ? `${parent.alias}.${relationLabel}` : `${sourceTable}.${relationLabel}`;

  const hop: JoinPathHop = {
    source: relation.sourceColumns.map((column) => `${sourceTable}.${column}`),
    target: relation.targetColumns.map((column) => `${targetTableName}.${column}`),
  };

  const path: JoinPathHop[] = parent ? [...parent.path, hop] : [hop];

  return {
    alias,
    table: targetTableName,
    path,
    depth,
    visitedTables,
  };
}

function collectSelectableColumns(tableSchema: Schema[number]) {
  return [
    ...tableSchema.columns.map((column) => column.name),
    ...(tableSchema.computedColumns ?? []).map((column) => column.name),
  ];
}
