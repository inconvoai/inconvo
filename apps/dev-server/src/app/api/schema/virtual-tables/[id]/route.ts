import { type NextRequest, NextResponse } from "next/server";
import { prisma } from "~/lib/prisma";
import { syncAugmentationsToFile } from "~/lib/schema";

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
      access?: string;
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
      include: { virtualTableConfig: true },
    });

    if (!existing) {
      return NextResponse.json(
        { error: "Virtual table not found" },
        { status: 404 },
      );
    }

    const { sql, dialect, access, context, columns } = body;

    // Update table-level fields
    await prisma.table.update({
      where: { id },
      data: {
        ...(access !== undefined && { access }),
        ...(context !== undefined && { context }),
      },
    });

    // Update virtual table config if sql or dialect changed
    if (sql !== undefined || dialect !== undefined) {
      if (existing.virtualTableConfig) {
        await prisma.virtualTableConfig.update({
          where: { tableId: id },
          data: {
            ...(sql !== undefined && { sql }),
            ...(dialect !== undefined && { dialect }),
          },
        });
      }
    }

    // Replace columns if provided
    if (columns !== undefined) {
      await prisma.column.deleteMany({ where: { tableId: id } });
      if (columns.length > 0) {
        await prisma.column.createMany({
          data: columns.map((col) => ({
            tableId: id,
            name: col.sourceName,
            rename: col.name !== col.sourceName ? col.name : null,
            type: col.type,
            selected: col.selected ?? true,
            unit: col.unit ?? null,
            notes: col.notes ?? null,
          })),
        });
      }
    }

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
