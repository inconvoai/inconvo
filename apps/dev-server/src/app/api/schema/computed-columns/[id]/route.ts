import { type NextRequest, NextResponse } from "next/server";
import { prisma } from "~/lib/prisma";
import { syncAugmentationsToFile } from "~/lib/schema";

interface RouteParams {
  params: Promise<{ id: string }>;
}

/**
 * PATCH /api/schema/computed-columns/[id]
 * Update a computed column
 */
export async function PATCH(request: NextRequest, { params }: RouteParams) {
  const { id } = await params;

  try {
    const body = (await request.json()) as {
      name?: string;
      selected?: boolean;
      notes?: string | null;
    };
    const { name, selected, notes } = body;

    const computedColumn = await prisma.computedColumn.update({
      where: { id },
      data: {
        ...(name !== undefined && { name }),
        ...(selected !== undefined && { selected }),
        ...(notes !== undefined && { notes }),
      },
    });

    // Sync augmentations to file
    await syncAugmentationsToFile();

    return NextResponse.json({ computedColumn });
  } catch (error) {
    console.error("Failed to update computed column:", error);
    return NextResponse.json(
      {
        error:
          error instanceof Error
            ? error.message
            : "Failed to update computed column",
      },
      { status: 500 },
    );
  }
}

/**
 * DELETE /api/schema/computed-columns/[id]
 * Delete a computed column
 */
export async function DELETE(_request: NextRequest, { params }: RouteParams) {
  const { id } = await params;

  try {
    await prisma.computedColumn.delete({
      where: { id },
    });

    // Sync augmentations to file
    await syncAugmentationsToFile();

    return NextResponse.json({ success: true });
  } catch (error) {
    console.error("Failed to delete computed column:", error);
    return NextResponse.json(
      {
        error:
          error instanceof Error
            ? error.message
            : "Failed to delete computed column",
      },
      { status: 500 },
    );
  }
}
