import { type NextRequest, NextResponse } from "next/server";
import { prisma } from "~/lib/prisma";
import { syncAugmentationsToFile } from "~/lib/schema";

interface RouteParams {
  params: Promise<{ id: string }>;
}

/**
 * PATCH /api/schema/conversions/[id]
 * Update a column conversion
 */
export async function PATCH(request: NextRequest, { params }: RouteParams) {
  const { id } = await params;

  try {
    const body = (await request.json()) as {
      type: string;
      ast: unknown;
      selected: boolean;
    };
    const { type, ast, selected } = body;

    const conversion = await prisma.columnConversion.update({
      where: { id },
      data: {
        type,
        ast: JSON.stringify(ast),
        selected,
      },
    });

    // Sync augmentations to file
    await syncAugmentationsToFile();

    return NextResponse.json({ conversion });
  } catch (error) {
    console.error("Failed to update column conversion:", error);
    return NextResponse.json(
      {
        error:
          error instanceof Error
            ? error.message
            : "Failed to update column conversion",
      },
      { status: 500 },
    );
  }
}

/**
 * DELETE /api/schema/conversions/[id]
 * Delete a column conversion
 */
export async function DELETE(_request: NextRequest, { params }: RouteParams) {
  const { id } = await params;

  try {
    await prisma.columnConversion.delete({
      where: { id },
    });

    // Sync augmentations to file
    await syncAugmentationsToFile();

    return NextResponse.json({ success: true });
  } catch (error) {
    console.error("Failed to delete column conversion:", error);
    return NextResponse.json(
      {
        error:
          error instanceof Error
            ? error.message
            : "Failed to delete column conversion",
      },
      { status: 500 },
    );
  }
}
