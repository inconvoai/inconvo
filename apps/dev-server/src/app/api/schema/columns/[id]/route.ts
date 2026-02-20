import { type NextRequest, NextResponse } from "next/server";
import { prisma } from "~/lib/prisma";
import { syncAugmentationsToFile } from "~/lib/schema";
import { findColumnRenameConflict } from "@repo/schema-utils";

interface RouteParams {
  params: Promise<{ id: string }>;
}

/**
 * PATCH /api/schema/columns/[id]
 * Update column settings (rename, notes, unit, selected)
 */
export async function PATCH(request: NextRequest, { params }: RouteParams) {
  const { id } = await params;

  try {
    const body = (await request.json()) as {
      rename?: string | null;
      notes?: string;
      unit?: string;
      selected?: boolean;
    };
    const { rename, notes, unit, selected } = body;

    const currentColumn = await prisma.column.findUnique({
      where: { id },
      select: {
        id: true,
        name: true,
        rename: true,
        selected: true,
        table: {
          select: {
            columns: {
              select: { id: true, name: true, rename: true },
            },
            computedColumns: {
              where: { selected: true },
              select: { name: true },
            },
          },
        },
      },
    });

    if (!currentColumn) {
      return NextResponse.json({ error: "Column not found" }, { status: 404 });
    }

    let normalizedRename: string | null | undefined = undefined;
    if (rename !== undefined) {
      if (rename === null) {
        normalizedRename = null;
      } else {
        const trimmedRename = rename.trim();
        if (trimmedRename.length === 0) {
          return NextResponse.json(
            { error: "Rename must not be empty" },
            { status: 400 },
          );
        }
        normalizedRename = trimmedRename;
      }

      const nextEffectiveName = normalizedRename ?? currentColumn.name;
      const conflict = findColumnRenameConflict({
        currentColumnId: currentColumn.id,
        nextEffectiveName,
        tableColumns: currentColumn.table.columns,
        computedColumns: currentColumn.table.computedColumns,
      });

      if (conflict) {
        const conflictSubject =
          conflict.type === "computedColumn"
            ? `computed column "${conflict.name}"`
            : `existing column "${conflict.name}"`;
        return NextResponse.json(
          {
            error: `Column rename "${nextEffectiveName}" conflicts with ${conflictSubject} in this table.`,
          },
          { status: 409 },
        );
      }
    }

    const column = await prisma.column.update({
      where: { id },
      data: {
        ...(normalizedRename !== undefined && { rename: normalizedRename }),
        ...(notes !== undefined && { notes }),
        ...(unit !== undefined && { unit }),
        ...(selected !== undefined && { selected }),
      },
    });

    const renameChanged =
      rename !== undefined && (currentColumn.rename ?? null) !== (column.rename ?? null);
    const selectedChanged =
      selected !== undefined && currentColumn.selected !== column.selected;
    const selectedAffectsRenames =
      selectedChanged &&
      ((currentColumn.rename ?? null) !== null || (column.rename ?? null) !== null);

    if (renameChanged || selectedAffectsRenames) {
      await syncAugmentationsToFile();
    }

    return NextResponse.json({ column });
  } catch (error) {
    console.error("Failed to update column:", error);
    return NextResponse.json(
      {
        error:
          error instanceof Error ? error.message : "Failed to update column",
      },
      { status: 500 },
    );
  }
}
