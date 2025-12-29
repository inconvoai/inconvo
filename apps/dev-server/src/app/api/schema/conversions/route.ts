import { type NextRequest, NextResponse } from "next/server";
import { prisma } from "~/lib/prisma";
import { syncAugmentationsToFile } from "~/lib/schema";

/**
 * POST /api/schema/conversions
 * Create a column conversion
 */
export async function POST(request: NextRequest) {
  try {
    const body = (await request.json()) as {
      columnId: string;
      type: string;
      ast: unknown;
      selected?: boolean;
    };
    const { columnId, type, ast, selected = true } = body;

    // Get the column to find its table
    const column = await prisma.column.findUnique({
      where: { id: columnId },
      select: { tableId: true },
    });

    if (!column) {
      return NextResponse.json({ error: "Column not found" }, { status: 404 });
    }

    // Check if conversion already exists
    const existingConversion = await prisma.columnConversion.findUnique({
      where: { columnId },
    });

    if (existingConversion) {
      return NextResponse.json(
        { error: "Conversion already exists for this column" },
        { status: 409 },
      );
    }

    const conversion = await prisma.columnConversion.create({
      data: {
        columnId,
        tableId: column.tableId,
        type,
        ast: JSON.stringify(ast),
        selected,
      },
    });

    // Sync augmentations to file
    await syncAugmentationsToFile();

    return NextResponse.json({ conversion });
  } catch (error) {
    console.error("Failed to create column conversion:", error);
    return NextResponse.json(
      {
        error:
          error instanceof Error
            ? error.message
            : "Failed to create column conversion",
      },
      { status: 500 },
    );
  }
}
