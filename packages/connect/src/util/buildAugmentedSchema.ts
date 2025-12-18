import type {
  SchemaResponse,
  SchemaRelation,
  SchemaTable,
} from "~/types/types";
import { getCachedSchema } from "./schemaCache";
import {
  readCustomRelationsAugmentation,
  readComputedColumnsAugmentation,
  readColumnConversionsAugmentation,
} from "./schemaAugmentationStore";
import { logger } from "~/util/logger";
import {
  SQLCastExpressionAstSchema,
  SQLComputedColumnAstSchema,
  type SQLCastExpressionAst,
} from "~/types/querySchema";

type RelationNameRegistry = Map<string, Map<string, number>>;

function cloneTables(base: SchemaResponse): SchemaTable[] {
  return base.tables.map((table) => ({
    ...table,
    relations: table.relations ? [...table.relations] : [],
    computedColumns: table.computedColumns
      ? [...table.computedColumns]
      : undefined,
    columnConversions: table.columnConversions
      ? [...table.columnConversions]
      : undefined,
  }));
}

function initRelationRegistry(tables: SchemaTable[]): RelationNameRegistry {
  const registry: RelationNameRegistry = new Map();
  tables.forEach((table) => {
    const map = new Map<string, number>();
    (table.relations ?? []).forEach((relation) => {
      const count = map.get(relation.name) ?? 0;
      map.set(relation.name, count + 1);
    });
    registry.set(table.name, map);
  });
  return registry;
}

function getUniqueRelationName(
  registry: RelationNameRegistry,
  tableName: string,
  desired: string
): string {
  if (!registry.has(tableName)) {
    registry.set(tableName, new Map());
  }
  const map = registry.get(tableName)!;
  const current = map.get(desired) ?? 0;
  map.set(desired, current + 1);
  if (current === 0) {
    return desired;
  }
  return `${desired}_${current}`;
}

function pluralize(tableName: string): string {
  if (tableName.endsWith("ies")) {
    return tableName;
  }
  if (
    tableName.endsWith("y") &&
    !["ay", "ey", "iy", "oy", "uy"].includes(tableName.slice(-2))
  ) {
    return `${tableName.slice(0, -1)}ies`;
  }
  if (
    tableName.endsWith("ss") ||
    tableName.endsWith("x") ||
    tableName.endsWith("ch") ||
    tableName.endsWith("sh")
  ) {
    return `${tableName}es`;
  }
  if (!tableName.endsWith("s")) {
    return `${tableName}s`;
  }
  return tableName;
}

function addRelationToTable(
  table: SchemaTable,
  relation: SchemaRelation & {
    source?: "FK" | "MANUAL";
    status?: "VALID" | "BROKEN";
    errorTag?: string | null;
  }
) {
  if (!table.relations) {
    table.relations = [];
  }
  table.relations.push(relation);
}

function columnsExist(
  table: SchemaTable,
  columns: string[],
  columnType: "source" | "target"
) {
  const available = new Set(table.columns.map((column) => column.name));
  const missing = columns.filter((column) => !available.has(column));
  if (missing.length) {
    logger.warn(
      {
        table: table.name,
        columnType,
        missing,
      },
      "Augmented schema - manual relation references missing columns"
    );
    return false;
  }
  return true;
}

function deriveSimpleTypeFromCast(ast: SQLCastExpressionAst): string | undefined {
  switch (ast.type) {
    case "cast": {
      const lowered = ast.as.toLowerCase();
      if (
        lowered.includes("int") ||
        lowered.includes("numeric") ||
        lowered.includes("decimal") ||
        lowered.includes("float") ||
        lowered.includes("double") ||
        lowered.includes("real")
      ) {
        return "number";
      }
      if (lowered.includes("bool")) {
        return "boolean";
      }
      if (lowered.includes("date") || lowered.includes("time")) {
        return "DateTime";
      }
      if (
        lowered.includes("char") ||
        lowered.includes("text") ||
        lowered.includes("string")
      ) {
        return "string";
      }
      break;
    }
    case "coalesce":
      return (
        deriveSimpleTypeFromCast(ast.expression) ??
        deriveSimpleTypeFromCast(ast.fallback)
      );
    case "brackets":
      return deriveSimpleTypeFromCast(ast.expression);
    default:
      return undefined;
  }
}

export async function buildAugmentedSchema(): Promise<SchemaResponse> {
  const [
    baseSchema,
    customRelations,
    computedColumnsAugmentation,
    columnConversionsAugmentation,
  ] = await Promise.all([
    getCachedSchema(),
    readCustomRelationsAugmentation(),
    readComputedColumnsAugmentation(),
    readColumnConversionsAugmentation(),
  ]);

  const tables = cloneTables(baseSchema);
  const tableMap = new Map(tables.map((table) => [table.name, table]));
  const relationRegistry = initRelationRegistry(tables);

  for (const relation of customRelations.relations ?? []) {
    if (relation.selected === false) {
      continue;
    }
    if (relation.status && relation.status !== "VALID") {
      continue;
    }

    const sourceTable = tableMap.get(relation.sourceTable);
    const targetTable = tableMap.get(relation.targetTable);
    if (!sourceTable || !targetTable) {
      logger.warn(
        {
          relation: relation.name,
          sourceTable: relation.sourceTable,
          targetTable: relation.targetTable,
        },
        "Augmented schema - manual relation references unknown table"
      );
      continue;
    }

    if (relation.sourceColumns.length !== relation.targetColumns.length) {
      logger.warn(
        {
          relation: relation.name,
          sourceTable: relation.sourceTable,
          targetTable: relation.targetTable,
        },
        "Augmented schema - manual relation column metadata mismatch"
      );
      continue;
    }

    const sourceColumnsValid = columnsExist(
      sourceTable,
      relation.sourceColumns,
      "source"
    );
    const targetColumnsValid = columnsExist(
      targetTable,
      relation.targetColumns,
      "target"
    );
    if (!sourceColumnsValid || !targetColumnsValid) {
      continue;
    }

    const uniqueRelationName = getUniqueRelationName(
      relationRegistry,
      sourceTable.name,
      relation.name
    );

    addRelationToTable(sourceTable, {
      name: uniqueRelationName,
      isList: relation.isList,
      targetTable: targetTable.name,
      sourceColumns: relation.sourceColumns,
      targetColumns: relation.targetColumns,
      source: "MANUAL",
      status: relation.status ?? "VALID",
      errorTag: relation.errorTag ?? null,
    });

    const reverseCandidate = pluralize(sourceTable.name);

    const reverseRelationName = getUniqueRelationName(
      relationRegistry,
      targetTable.name,
      reverseCandidate
    );

    addRelationToTable(targetTable, {
      name: reverseRelationName,
      isList: !relation.isList,
      targetTable: sourceTable.name,
      sourceColumns: relation.targetColumns,
      targetColumns: relation.sourceColumns,
      source: "MANUAL",
      status: relation.status ?? "VALID",
      errorTag: relation.errorTag ?? null,
    });
  }

  const computedColumns = computedColumnsAugmentation.computedColumns ?? [];
  let attachedComputedColumns = 0;

  for (const computed of computedColumns) {
    if (computed.selected === false) {
      continue;
    }

    const targetTable = tableMap.get(computed.table);
    if (!targetTable) {
      logger.warn(
        {
          computedColumn: computed.name,
          table: computed.table,
        },
        "Augmented schema - computed column references unknown table"
      );
      continue;
    }

    const parsedAst = SQLComputedColumnAstSchema.safeParse(computed.ast);
    if (!parsedAst.success) {
      logger.warn(
        {
          computedColumn: computed.name,
          table: computed.table,
          issues: parsedAst.error.issues,
        },
        "Augmented schema - computed column has invalid AST"
      );
      continue;
    }

    if (!targetTable.computedColumns) {
      targetTable.computedColumns = [];
    }

    const normalized = {
      name: computed.name,
      ast: parsedAst.data,
      type: computed.type ?? null,
      unit: computed.unit ?? null,
      notes: computed.notes ?? null,
    };

    const existingIndex = targetTable.computedColumns.findIndex(
      (column) => column.name === normalized.name
    );
    if (existingIndex >= 0) {
      targetTable.computedColumns[existingIndex] = normalized;
    } else {
      targetTable.computedColumns.push(normalized);
    }

    attachedComputedColumns += 1;
  }

  if (attachedComputedColumns > 0) {
    logger.debug(
      {
        computedColumns: attachedComputedColumns,
      },
      "Augmented schema - computed columns augmentation merged"
    );
  }

  const columnConversions = columnConversionsAugmentation.columnConversions ?? [];
  let attachedColumnConversions = 0;

  for (const conversion of columnConversions) {
    if (conversion.selected === false) {
      continue;
    }

    const targetTable = tableMap.get(conversion.table);
    if (!targetTable) {
      logger.warn(
        {
          column: conversion.column,
          table: conversion.table,
        },
        "Augmented schema - column conversion references unknown table"
      );
      continue;
    }

    const parsedAst = SQLCastExpressionAstSchema.safeParse(conversion.ast);
    if (!parsedAst.success) {
      logger.warn(
        {
          column: conversion.column,
          table: conversion.table,
          issues: parsedAst.error.issues,
        },
        "Augmented schema - column conversion has invalid AST"
      );
      continue;
    }

    const targetColumn = targetTable.columns.find(
      (col) => col.name === conversion.column
    );
    if (!targetColumn) {
      logger.warn(
        {
          column: conversion.column,
          table: conversion.table,
        },
        "Augmented schema - column conversion references unknown column"
      );
      continue;
    }

    if (!targetTable.columnConversions) {
      targetTable.columnConversions = [];
    }

    const derivedType =
      conversion.type ?? deriveSimpleTypeFromCast(parsedAst.data);

    const normalized = {
      column: conversion.column,
      ast: parsedAst.data,
      type: derivedType ?? null,
    };

    const existingIndex = targetTable.columnConversions.findIndex(
      (col) => col.column === normalized.column
    );
    if (existingIndex >= 0) {
      targetTable.columnConversions[existingIndex] = normalized;
    } else {
      targetTable.columnConversions.push(normalized);
    }

    if (derivedType) {
      targetColumn.type = derivedType;
    }

    attachedColumnConversions += 1;
  }

  if (attachedColumnConversions > 0) {
    logger.debug(
      {
        columnConversions: attachedColumnConversions,
      },
      "Augmented schema - column conversions augmentation merged"
    );
  }

  return {
    ...baseSchema,
    tables,
  };
}
