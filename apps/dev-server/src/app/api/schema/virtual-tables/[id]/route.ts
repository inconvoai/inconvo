import { type NextRequest, NextResponse } from "next/server";
import { prisma } from "~/lib/prisma";
import { syncAugmentationsToFile } from "~/lib/schema";
import {
  getDatabaseDialectErrorMessage,
  isDatabaseDialect,
  validateVirtualTableSql,
} from "~/lib/virtualTableValidation";

interface RouteParams {
  params: Promise<{ id: string }>;
}

/**
 * GET /api/schema/virtual-tables/[id]
 * Get a single virtual table with full config and columns.
 */
export async function GET(_request: NextRequest, { params }: RouteParams) {
  const { id } = await params;

  try {
    const table = await prisma.table.findUnique({
      where: { id, source: "VIRTUAL" },
      include: {
        virtualTableConfig: true,
        columns: { orderBy: { name: "asc" } },
        outwardRelations: {
          include: {
            columnMappings: { orderBy: { position: "asc" } },
            targetTable: { select: { name: true } },
          },
          orderBy: { name: "asc" },
        },
        condition: {
          include: {
            column: { select: { name: true } },
            userContextField: { select: { key: true } },
          },
        },
        accessPolicy: {
          include: { userContextField: { select: { key: true } } },
        },
      },
    });

    if (!table) {
      return NextResponse.json(
        { error: "Virtual table not found" },
        { status: 404 },
      );
    }

    return NextResponse.json({ table });
  } catch (error) {
    console.error("Failed to get virtual table:", error);
    return NextResponse.json(
      {
        error:
          error instanceof Error
            ? error.message
            : "Failed to get virtual table",
      },
      { status: 500 },
    );
  }
}

/**
 * PATCH /api/schema/virtual-tables/[id]
 * Update a virtual table's SQL, columns, access, or context.
 *
 * Body: { sql?, dialect?, access?, context?, columns? }
 */
export async function PATCH(request: NextRequest, { params }: RouteParams) {
  const { id } = await params;

  try {
    const body = (await request.json()) as {
      sql?: string;
      dialect?: string;
      access?: "QUERYABLE" | "JOINABLE" | "OFF";
      context?: string | null;
      columns?: Array<{
        sourceName: string;
        name?: string;
        type: string;
        selected?: boolean;
        unit?: string | null;
        notes?: string | null;
      }>;
    };

    const existing = await prisma.table.findUnique({
      where: { id, source: "VIRTUAL" },
      include: {
        virtualTableConfig: true,
        columns: {
          orderBy: { name: "asc" },
        },
      },
    });

    if (!existing) {
      return NextResponse.json(
        { error: "Virtual table not found" },
        { status: 404 },
      );
    }
    if (!existing.virtualTableConfig) {
      return NextResponse.json(
        { error: "Virtual table configuration is missing." },
        { status: 400 },
      );
    }

    const { sql, dialect, access, context, columns } = body;
    const validAccess = ["QUERYABLE", "JOINABLE", "OFF"] as const;
    if (dialect && !isDatabaseDialect(dialect)) {
      return NextResponse.json(
        { error: getDatabaseDialectErrorMessage() },
        { status: 400 },
      );
    }
    if (access && !validAccess.includes(access)) {
      return NextResponse.json(
        { error: `access must be one of: ${validAccess.join(", ")}` },
        { status: 400 },
      );
    }

    let nextDialect = existing.virtualTableConfig?.dialect;
    let inferredColumns:
      | Array<{
          sourceName: string;
          name?: string;
          type: string;
          selected?: boolean;
          unit?: string | null;
          notes?: string | null;
        }>
      | undefined;

    const shouldValidateSql = sql !== undefined || dialect !== undefined;
    if (shouldValidateSql) {
      const sqlToValidate = sql ?? existing.virtualTableConfig.sql;
      if (!sqlToValidate) {
        return NextResponse.json(
          { error: "Virtual table SQL configuration is missing." },
          { status: 400 },
        );
      }

      const { dialect: validatedDialect, result } = await validateVirtualTableSql({
        sql: sqlToValidate,
        requestDialect: dialect,
      });
      if (!result.ok) {
        return NextResponse.json({ error: result.error.message }, { status: 400 });
      }
      if (result.columns.length === 0) {
        return NextResponse.json(
          {
            error:
              "Validation returned no columns. The query may have produced zero rows during validation; add a filter/limit for validation or use data that returns at least one row.",
          },
          { status: 400 },
        );
      }

      nextDialect = validatedDialect;

      const existingBySourceName = new Map(
        existing.columns.map((column) => [column.name, column] as const),
      );
      inferredColumns = result.columns.map((column) => {
        const existingColumn = existingBySourceName.get(column.sourceName);
        return {
          sourceName: column.sourceName,
          name: existingColumn?.rename ?? column.sourceName,
          type: column.type,
          selected: existingColumn?.selected ?? true,
          unit: existingColumn?.unit ?? null,
          notes: existingColumn?.notes ?? null,
        };
      });
    }

    const nextColumns = shouldValidateSql ? inferredColumns : columns;

    await prisma.$transaction(async (tx) => {
      // Update table-level fields
      await tx.table.update({
        where: { id },
        data: {
          ...(access !== undefined && { access }),
          ...(context !== undefined && { context }),
        },
      });

      // Update virtual table config if sql or dialect changed
      if (sql !== undefined || dialect !== undefined) {
        if (existing.virtualTableConfig) {
          await tx.virtualTableConfig.update({
            where: { tableId: id },
            data: {
              ...(sql !== undefined && { sql }),
              ...(nextDialect !== undefined && { dialect: nextDialect }),
            },
          });
        }
      }

      // Sync columns if provided, preserving IDs for unchanged source names.
      if (nextColumns !== undefined) {
        const existingBySourceName = new Map(
          existing.columns.map((column) => [column.name, column] as const),
        );
        const nextSourceNames = new Set(
          nextColumns.map((column) => column.sourceName),
        );

        const removedColumnIds = existing.columns
          .filter((column) => !nextSourceNames.has(column.name))
          .map((column) => column.id);

        if (removedColumnIds.length > 0) {
          await tx.column.deleteMany({
            where: { id: { in: removedColumnIds } },
          });
        }

        for (const column of nextColumns) {
          const columnData = {
            rename:
              column.name !== column.sourceName ? column.name : null,
            type: column.type,
            selected: column.selected ?? true,
            unit: column.unit ?? null,
            notes: column.notes ?? null,
          };
          const existingColumn = existingBySourceName.get(column.sourceName);

          if (existingColumn) {
            await tx.column.update({
              where: { id: existingColumn.id },
              data: columnData,
            });
            continue;
          }

          await tx.column.create({
            data: {
              tableId: id,
              name: column.sourceName,
              ...columnData,
            },
          });
        }
      }
    });

    const updated = await prisma.table.findUnique({
      where: { id },
      include: {
        virtualTableConfig: true,
        columns: { orderBy: { name: "asc" } },
      },
    });

    await syncAugmentationsToFile();

    return NextResponse.json({ table: updated });
  } catch (error) {
    console.error("Failed to update virtual table:", error);
    return NextResponse.json(
      {
        error:
          error instanceof Error
            ? error.message
            : "Failed to update virtual table",
      },
      { status: 500 },
    );
  }
}

/**
 * DELETE /api/schema/virtual-tables/[id]
 * Delete a virtual table and all associated data.
 */
export async function DELETE(_request: NextRequest, { params }: RouteParams) {
  const { id } = await params;

  try {
    const existing = await prisma.table.findUnique({
      where: { id, source: "VIRTUAL" },
    });

    if (!existing) {
      return NextResponse.json(
        { error: "Virtual table not found" },
        { status: 404 },
      );
    }

    await prisma.table.delete({ where: { id } });
    await syncAugmentationsToFile();

    return NextResponse.json({ success: true });
  } catch (error) {
    console.error("Failed to delete virtual table:", error);
    return NextResponse.json(
      {
        error:
          error instanceof Error
            ? error.message
            : "Failed to delete virtual table",
      },
      { status: 500 },
    );
  }
}
