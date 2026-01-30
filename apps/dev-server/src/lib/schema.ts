import type {
  Schema,
  SchemaColumn,
  SchemaRelation,
  SchemaComputedColumn,
  SQLCastExpressionAst,
  SQLComputedColumnAst,
} from "@repo/types";

// Matches the internal ColumnRelationEntry type in @repo/types
type ColumnRelationEntry = {
  relation: {
    targetTable: { name: string | null };
  };
  targetColumn: { name: string | null };
};
import type { SchemaResponse } from "@repo/connect";
import {
  writeUnifiedAugmentation,
  computeAugmentationsHash,
} from "@repo/connect";
import { prisma } from "./prisma";

/**
 * Resolve effective column type considering conversion.
 * Matches platform's resolveEffectiveColumnType.
 */
function resolveEffectiveColumnType(
  columnType: string,
  conversion?: { selected?: boolean | null; type?: string | null } | null,
): string {
  if (conversion?.selected && conversion.type) {
    return conversion.type;
  }
  return columnType;
}

/**
 * Get the database schema for the agent.
 * Queries from Prisma SQLite with filtering - matches platform's getSchema.
 *
 * Filtering behavior (matches platform):
 * - Only includes tables not marked as OFF
 * - Only includes tables with at least one selected column or relation
 * - Only includes columns that are selected
 * - Only includes relations that are selected, VALID, and target accessible tables
 */
export async function getSchema(): Promise<Schema> {
  const tables = await prisma.table.findMany({
    where: {
      access: { not: "OFF" },
      OR: [
        { columns: { some: { selected: true } } },
        { outwardRelations: { some: { selected: true } } },
      ],
    },
    select: {
      name: true,
      schema: true,
      access: true,
      context: true,
      columns: {
        select: {
          name: true,
          rename: true,
          notes: true,
          type: true,
          unit: true,
          conversion: {
            select: {
              id: true,
              ast: true,
              type: true,
              selected: true,
            },
          },
          // Get FK mappings where this column is the source
          sourceRelationMappings: {
            select: {
              targetColumnName: true,
              targetColumn: { select: { name: true } },
              relation: {
                select: {
                  targetTable: { select: { name: true, access: true } },
                  selected: true,
                  status: true,
                },
              },
            },
            where: {
              relation: {
                targetTable: { access: { not: "OFF" } },
                selected: true,
                status: "VALID",
              },
            },
          },
        },
        orderBy: { name: "asc" },
        where: { selected: true },
      },
      computedColumns: {
        select: {
          name: true,
          table: { select: { name: true } },
          type: true,
          ast: true,
          unit: true,
          notes: true,
        },
        where: { selected: true },
      },
      outwardRelations: {
        select: {
          name: true,
          targetTable: { select: { name: true, schema: true } },
          isList: true,
          selected: true,
          source: true,
          status: true,
          errorTag: true,
          columnMappings: {
            select: {
              position: true,
              sourceColumn: { select: { name: true } },
              targetColumn: { select: { name: true } },
              sourceColumnName: true,
              targetColumnName: true,
            },
            orderBy: { position: "asc" },
          },
        },
        orderBy: { name: "asc" },
        where: {
          targetTable: { access: { not: "OFF" } },
          selected: true,
          status: "VALID",
        },
      },
      condition: {
        select: {
          column: { select: { name: true } },
          userContextField: { select: { key: true } },
        },
      },
    },
    orderBy: { name: "asc" },
  });

  // Format to match platform's output shape exactly
  const schema: Schema = tables.map((table) => {
    // Format columns with relation mappings
    const columns: SchemaColumn[] = table.columns.map((column) => {
      // Build relation array from source relation mappings
      const relation: ColumnRelationEntry[] = (
        column.sourceRelationMappings ?? []
      ).map((mapping) => ({
        relation: {
          targetTable: {
            name: mapping.relation?.targetTable?.name ?? null,
          },
        },
        targetColumn: {
          name: mapping.targetColumn?.name ?? mapping.targetColumnName ?? null,
        },
      }));

      const conversion = column.conversion;
      const effectiveType = resolveEffectiveColumnType(column.type, conversion);

      return {
        name: column.rename ?? column.name,
        dbName: column.name,
        rename: column.rename ?? null,
        notes: column.notes ?? null,
        type: column.type,
        effectiveType,
        conversion: conversion
          ? {
              id: conversion.id,
              ast: JSON.parse(conversion.ast) as SQLCastExpressionAst,
              type: conversion.type ?? null,
              selected: conversion.selected,
            }
          : null,
        unit: column.unit ?? null,
        relation,
      };
    });

    // Format computed columns
    const computedColumns: SchemaComputedColumn[] = (
      table.computedColumns ?? []
    ).map((cc) => ({
      name: cc.name,
      table: { name: cc.table.name },
      type: cc.type,
      ast: JSON.parse(cc.ast) as SQLComputedColumnAst,
      unit: cc.unit ?? null,
      notes: cc.notes ?? null,
    }));

    // Format outward relations with column mappings
    const outwardRelations: SchemaRelation[] = (
      table.outwardRelations ?? []
    ).map((relation) => {
      const mappings = [...(relation.columnMappings ?? [])].sort(
        (a, b) => a.position - b.position,
      );
      const sourceColumns = mappings
        .map((m) => m.sourceColumn?.name ?? m.sourceColumnName)
        .filter((name): name is string =>
          Boolean(name && name.trim().length > 0),
        );
      const targetColumns = mappings
        .map((m) => m.targetColumn?.name ?? m.targetColumnName)
        .filter((name): name is string =>
          Boolean(name && name.trim().length > 0),
        );

      return {
        name: relation.name,
        relationId: null, // Dev server doesn't use relationId
        targetTable: { name: relation.targetTable.name },
        targetSchema: relation.targetTable.schema ?? null,
        isList: relation.isList,
        selected: relation.selected,
        source: relation.source as "FK" | "MANUAL",
        status: relation.status as "VALID" | "BROKEN",
        errorTag: relation.errorTag ?? null,
        sourceColumns,
        targetColumns,
      };
    });

    return {
      name: table.name,
      schema: table.schema ?? null,
      access: table.access as "QUERYABLE" | "JOINABLE" | "OFF",
      context: table.context ?? null,
      columns,
      computedColumns,
      outwardRelations,
      condition: table.condition
        ? {
            column: { name: table.condition.column.name },
            userContextField: {
              key: table.condition.userContextField.key,
            },
          }
        : null,
    };
  });

  return schema;
}

/**
 * Sync introspected schema to the local database.
 * Creates new tables/columns, updates existing ones, handles deletions.
 * Matches platform's syncSchema behavior.
 */
export async function syncSchema(): Promise<{
  added: number;
  updated: number;
}> {
  const { buildSchema } = await import("@repo/connect");
  const schemaResponse: SchemaResponse = await buildSchema();

  let added = 0;
  let updated = 0;

  // Fetch existing tables
  const existingTables = await prisma.table.findMany({
    include: {
      columns: true,
      outwardRelations: {
        include: { columnMappings: true },
      },
    },
  });

  const existingTableMap = new Map(existingTables.map((t) => [t.name, t]));
  const schemaTableNames = new Set(schemaResponse.tables.map((t) => t.name));

  // Delete obsolete tables (tables that no longer exist in DB)
  const tablesToDelete = existingTables
    .filter((t) => !schemaTableNames.has(t.name))
    .map((t) => t.id);

  if (tablesToDelete.length > 0) {
    await prisma.table.deleteMany({
      where: { id: { in: tablesToDelete } },
    });
  }

  // Process each table from introspected schema
  for (const table of schemaResponse.tables) {
    const existingTable = existingTableMap.get(table.name);

    if (!existingTable) {
      // Create new table with columns - default to QUERYABLE for new tables
      await prisma.table.create({
        data: {
          name: table.name,
          schema: table.schema ?? null,
          access: "QUERYABLE",
          columns: {
            create: table.columns.map((col) => ({
              name: col.name,
              type: col.type,
              selected: true,
            })),
          },
        },
      });
      added++;
    } else {
      // Update existing table - handle schema and column changes
      // Update schema if changed
      if (existingTable.schema !== (table.schema ?? null)) {
        await prisma.table.update({
          where: { id: existingTable.id },
          data: { schema: table.schema ?? null },
        });
      }

      const existingColumnNames = new Set(
        existingTable.columns.map((c) => c.name),
      );
      const schemaColumnNames = new Set(table.columns.map((c) => c.name));

      // Delete columns that no longer exist
      const columnsToDelete = existingTable.columns
        .filter((c) => !schemaColumnNames.has(c.name))
        .map((c) => c.id);

      if (columnsToDelete.length > 0) {
        await prisma.column.deleteMany({
          where: { id: { in: columnsToDelete } },
        });
      }

      // Add new columns
      for (const col of table.columns) {
        if (!existingColumnNames.has(col.name)) {
          await prisma.column.create({
            data: {
              name: col.name,
              type: col.type,
              selected: true,
              tableId: existingTable.id,
            },
          });
        } else {
          // Update type if changed
          await prisma.column.updateMany({
            where: { tableId: existingTable.id, name: col.name },
            data: { type: col.type },
          });
        }
      }
      updated++;
    }
  }

  // Sync FK relations
  const upsertedTables = await prisma.table.findMany();
  const tableIdMap = new Map(upsertedTables.map((t) => [t.name, t.id]));

  // Collect new relation identifiers
  const newRelationIds = new Set<string>();
  for (const table of schemaResponse.tables) {
    for (const rel of table.relations ?? []) {
      newRelationIds.add(`${table.name}:${rel.name}`);
    }
  }

  // Delete obsolete FK relations
  for (const existingTable of existingTables) {
    for (const existingRel of existingTable.outwardRelations) {
      const relId = `${existingTable.name}:${existingRel.name}`;
      if (existingRel.source === "FK" && !newRelationIds.has(relId)) {
        await prisma.relation.delete({ where: { id: existingRel.id } });
      }
    }
  }

  // Upsert FK relations
  for (const table of schemaResponse.tables) {
    const sourceTableId = tableIdMap.get(table.name);
    if (!sourceTableId) continue;

    for (const rel of table.relations ?? []) {
      const targetTableId = tableIdMap.get(rel.targetTable);
      if (!targetTableId) continue;

      const existingRelation = await prisma.relation.findFirst({
        where: { sourceTableId, name: rel.name },
      });

      if (existingRelation) {
        // Update existing relation
        await prisma.relation.update({
          where: { id: existingRelation.id },
          data: {
            isList: rel.isList,
            targetTableId,
            source: "FK",
            status: "VALID",
            errorTag: null,
          },
        });

        // Update column mappings
        await prisma.relationColumnMapping.deleteMany({
          where: { relationId: existingRelation.id },
        });

        const sourceColumns = rel.sourceColumns ?? [];
        const targetColumns = rel.targetColumns ?? [];

        for (let idx = 0; idx < sourceColumns.length; idx++) {
          const sourceColumnName = sourceColumns[idx];
          const targetColumnName = targetColumns[idx];
          if (!sourceColumnName || !targetColumnName) continue;

          // Find column IDs
          const sourceColumn = await prisma.column.findFirst({
            where: { tableId: sourceTableId, name: sourceColumnName },
          });
          const targetColumn = await prisma.column.findFirst({
            where: { tableId: targetTableId, name: targetColumnName },
          });

          await prisma.relationColumnMapping.create({
            data: {
              relationId: existingRelation.id,
              position: idx,
              sourceColumnName,
              targetColumnName,
              sourceColumnId: sourceColumn?.id ?? null,
              targetColumnId: targetColumn?.id ?? null,
            },
          });
        }
      } else {
        // Create new relation
        const newRelation = await prisma.relation.create({
          data: {
            name: rel.name,
            isList: rel.isList,
            sourceTableId,
            targetTableId,
            source: "FK",
            status: "VALID",
          },
        });

        // Create column mappings
        const sourceColumns = rel.sourceColumns ?? [];
        const targetColumns = rel.targetColumns ?? [];

        for (let idx = 0; idx < sourceColumns.length; idx++) {
          const sourceColumnName = sourceColumns[idx];
          const targetColumnName = targetColumns[idx];
          if (!sourceColumnName || !targetColumnName) continue;

          const sourceColumn = await prisma.column.findFirst({
            where: { tableId: sourceTableId, name: sourceColumnName },
          });
          const targetColumn = await prisma.column.findFirst({
            where: { tableId: targetTableId, name: targetColumnName },
          });

          await prisma.relationColumnMapping.create({
            data: {
              relationId: newRelation.id,
              position: idx,
              sourceColumnName,
              targetColumnName,
              sourceColumnId: sourceColumn?.id ?? null,
              targetColumnId: targetColumn?.id ?? null,
            },
          });
        }
      }
    }
  }

  // Validate manual relations
  const manualRelations = await prisma.relation.findMany({
    where: { source: "MANUAL" },
    include: { columnMappings: true },
  });

  for (const rel of manualRelations) {
    let status = "VALID";
    let errorTag: string | null = null;
    let selected = rel.selected;

    if (rel.columnMappings.length === 0) {
      status = "BROKEN";
      errorTag = "missing-columns";
    } else {
      const missingMapping = rel.columnMappings.find(
        (m) => !m.sourceColumnId || !m.targetColumnId,
      );
      if (missingMapping) {
        status = "BROKEN";
        const missingName = !missingMapping.sourceColumnId
          ? missingMapping.sourceColumnName
          : missingMapping.targetColumnName;
        errorTag = `missing-column ${missingName}`;
      }
    }

    if (status === "BROKEN" && rel.selected) {
      selected = false;
    }

    if (
      status !== rel.status ||
      errorTag !== rel.errorTag ||
      selected !== rel.selected
    ) {
      await prisma.relation.update({
        where: { id: rel.id },
        data: { status, errorTag, selected },
      });
    }
  }

  // Sync augmentations to file after schema sync
  await syncAugmentationsToFile();

  return { added, updated };
}

/**
 * Get list of all tables with their configuration status.
 * Used by the UI to show table list.
 * Only queries from local SQLite - does NOT introspect the database.
 */
export async function getTablesOverview() {
  const tables = await prisma.table.findMany({
    include: {
      _count: {
        select: {
          columns: true,
          computedColumns: true,
          outwardRelations: true,
        },
      },
      condition: true,
    },
    orderBy: { name: "asc" },
  });

  return tables.map((table) => ({
    id: table.id,
    name: table.name,
    columnCount: table._count.columns,
    relationCount: table._count.outwardRelations,
    configured: true,
    access: table.access,
    hasCondition: !!table.condition,
    computedColumnCount: table._count.computedColumns,
  }));
}

/**
 * Get table IDs with filtering and pagination.
 * Matches platform's getTableIds query.
 */
export async function getTableIds(params: {
  access?: ("QUERYABLE" | "JOINABLE" | "OFF")[];
  searchQuery?: string;
  pagination?: { page: number; perPage: number };
}) {
  const { access, searchQuery, pagination } = params;

  const where: {
    access?: { in: string[] };
    name?: { contains: string };
  } = {};

  if (access && access.length > 0) {
    where.access = { in: access };
  }

  if (searchQuery) {
    where.name = { contains: searchQuery };
  }

  const tables = await prisma.table.findMany({
    where,
    select: {
      id: true,
      name: true,
      access: true,
    },
    orderBy: { name: "asc" },
    skip: pagination ? (pagination.page - 1) * pagination.perPage : undefined,
    take: pagination?.perPage,
  });

  return tables;
}

/**
 * Get total count of tables with filtering.
 * Used for pagination.
 */
export async function getTableCount(params: {
  access?: ("QUERYABLE" | "JOINABLE" | "OFF")[];
  searchQuery?: string;
}) {
  const { access, searchQuery } = params;

  const where: {
    access?: { in: string[] };
    name?: { contains: string };
  } = {};

  if (access && access.length > 0) {
    where.access = { in: access };
  }

  if (searchQuery) {
    where.name = { contains: searchQuery };
  }

  return prisma.table.count({ where });
}

/**
 * Sync augmentations from SQLite to augmentations.json file.
 * This exports manual relations, computed columns, and column conversions
 * to the file format that the connect package reads.
 */
export async function syncAugmentationsToFile(): Promise<void> {
  // Fetch manual relations with their column mappings
  const manualRelations = await prisma.relation.findMany({
    where: { source: "MANUAL" },
    include: {
      sourceTable: { select: { name: true } },
      targetTable: { select: { name: true } },
      columnMappings: {
        select: {
          sourceColumnName: true,
          targetColumnName: true,
        },
        orderBy: { position: "asc" },
      },
    },
  });

  // Fetch computed columns
  const computedColumns = await prisma.computedColumn.findMany({
    include: {
      table: { select: { name: true } },
    },
  });

  // Fetch column conversions
  const columnConversions = await prisma.columnConversion.findMany({
    include: {
      column: { select: { name: true } },
      table: { select: { name: true } },
    },
  });

  // Transform to unified augmentation format
  const relations = manualRelations.map((rel) => ({
    name: rel.name,
    isList: rel.isList,
    sourceTable: rel.sourceTable.name,
    targetTable: rel.targetTable.name,
    sourceColumns: rel.columnMappings.map((m) => m.sourceColumnName),
    targetColumns: rel.columnMappings.map((m) => m.targetColumnName),
    selected: rel.selected,
    status: rel.status as "VALID" | "BROKEN",
    errorTag: rel.errorTag,
  }));

  const computedColumnsPayload = computedColumns.map((cc) => ({
    name: cc.name,
    table: cc.table.name,
    ast: JSON.parse(cc.ast) as SQLComputedColumnAst,
    type: cc.type,
    unit: cc.unit,
    notes: cc.notes,
    selected: cc.selected,
  }));

  const columnConversionsPayload = columnConversions.map((conv) => ({
    column: conv.column.name,
    table: conv.table.name,
    ast: JSON.parse(conv.ast) as SQLCastExpressionAst,
    type: conv.type ?? undefined,
    selected: conv.selected,
  }));

  const payload = {
    relations,
    computedColumns: computedColumnsPayload,
    columnConversions: columnConversionsPayload,
  };

  // Compute hash and write to file
  const hash = computeAugmentationsHash(payload);

  await writeUnifiedAugmentation({
    updatedAt: new Date().toISOString(),
    hash,
    relations,
    computedColumns: computedColumnsPayload,
    columnConversions: columnConversionsPayload,
  });
}
