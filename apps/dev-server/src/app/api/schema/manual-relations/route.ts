import { type NextRequest, NextResponse } from "next/server";
import { prisma } from "~/lib/prisma";
import { syncAugmentationsToFile } from "~/lib/schema";

/**
 * POST /api/schema/manual-relations
 * Create a manual relation
 */
export async function POST(request: NextRequest) {
  try {
    const body = (await request.json()) as {
      sourceTableId: string;
      name: string;
      isList: boolean;
      targetTableId: string;
      columnPairs: Array<{
        sourceColumnName: string;
        targetColumnName: string;
      }>;
    };
    const { sourceTableId, name, isList, targetTableId, columnPairs } = body;

    // Verify tables exist
    const [sourceTable, targetTable] = await Promise.all([
      prisma.table.findUnique({ where: { id: sourceTableId } }),
      prisma.table.findUnique({ where: { id: targetTableId } }),
    ]);

    if (!sourceTable || !targetTable) {
      return NextResponse.json(
        { error: "Source or target table not found" },
        { status: 404 },
      );
    }

    // Get column IDs for the mappings
    const sourceColumns = await prisma.column.findMany({
      where: {
        tableId: sourceTableId,
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

    // Create the relation with column mappings
    const relation = await prisma.relation.create({
      data: {
        name,
        isList,
        sourceTableId,
        targetTableId,
        source: "MANUAL",
        status: "VALID",
        selected: true,
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
    console.error("Failed to create manual relation:", error);
    return NextResponse.json(
      {
        error:
          error instanceof Error
            ? error.message
            : "Failed to create manual relation",
      },
      { status: 500 },
    );
  }
}
