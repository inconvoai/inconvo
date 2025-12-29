import { type NextRequest, NextResponse } from "next/server";
import { prisma } from "~/lib/prisma";
import { syncAugmentationsToFile } from "~/lib/schema";

interface RouteParams {
  params: Promise<{ id: string }>;
}

/**
 * PATCH /api/schema/manual-relations/[id]
 * Update a manual relation
 */
export async function PATCH(request: NextRequest, { params }: RouteParams) {
  const { id } = await params;

  try {
    const body = (await request.json()) as {
      name: string;
      isList: boolean;
      targetTableId: string;
      columnPairs: Array<{
        sourceColumnName: string;
        targetColumnName: string;
      }>;
    };
    const { name, isList, targetTableId, columnPairs } = body;

    // Get current relation to find source table
    const existingRelation = await prisma.relation.findUnique({
      where: { id },
      select: { sourceTableId: true, source: true },
    });

    if (!existingRelation) {
      return NextResponse.json(
        { error: "Relation not found" },
        { status: 404 },
      );
    }

    if (existingRelation.source !== "MANUAL") {
      return NextResponse.json(
        { error: "Can only update manual relations" },
        { status: 400 },
      );
    }

    // Get column IDs for the mappings
    const sourceColumns = await prisma.column.findMany({
      where: {
        tableId: existingRelation.sourceTableId,
        name: { in: columnPairs.map((p) => p.sourceColumnName) },
      },
    });
    const targetColumns = await prisma.column.findMany({
      where: {
        tableId: targetTableId,
        name: { in: columnPairs.map((p) => p.targetColumnName) },
      },
    });

    const sourceColMap = new Map(sourceColumns.map((c) => [c.name, c.id]));
    const targetColMap = new Map(targetColumns.map((c) => [c.name, c.id]));

    // Delete existing column mappings and update relation
    await prisma.relationColumnMapping.deleteMany({
      where: { relationId: id },
    });

    const relation = await prisma.relation.update({
      where: { id },
      data: {
        name,
        isList,
        targetTableId,
        columnMappings: {
          create: columnPairs.map((pair, idx) => ({
            position: idx,
            sourceColumnName: pair.sourceColumnName,
            targetColumnName: pair.targetColumnName,
            sourceColumnId: sourceColMap.get(pair.sourceColumnName) ?? null,
            targetColumnId: targetColMap.get(pair.targetColumnName) ?? null,
          })),
        },
      },
      include: {
        columnMappings: true,
        targetTable: true,
      },
    });

    // Sync augmentations to file
    await syncAugmentationsToFile();

    return NextResponse.json({ relation });
  } catch (error) {
    console.error("Failed to update manual relation:", error);
    return NextResponse.json(
      {
        error:
          error instanceof Error
            ? error.message
            : "Failed to update manual relation",
      },
      { status: 500 },
    );
  }
}

/**
 * DELETE /api/schema/manual-relations/[id]
 * Delete a manual relation
 */
export async function DELETE(_request: NextRequest, { params }: RouteParams) {
  const { id } = await params;

  try {
    // Verify it's a manual relation
    const relation = await prisma.relation.findUnique({
      where: { id },
      select: { source: true },
    });

    if (!relation) {
      return NextResponse.json(
        { error: "Relation not found" },
        { status: 404 },
      );
    }

    if (relation.source !== "MANUAL") {
      return NextResponse.json(
        { error: "Can only delete manual relations" },
        { status: 400 },
      );
    }

    await prisma.relation.delete({
      where: { id },
    });

    // Sync augmentations to file
    await syncAugmentationsToFile();

    return NextResponse.json({ success: true });
  } catch (error) {
    console.error("Failed to delete manual relation:", error);
    return NextResponse.json(
      {
        error:
          error instanceof Error
            ? error.message
            : "Failed to delete manual relation",
      },
      { status: 500 },
    );
  }
}
