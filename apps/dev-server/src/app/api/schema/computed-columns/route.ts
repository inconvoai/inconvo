import { type NextRequest, NextResponse } from "next/server";
import { prisma } from "~/lib/prisma";
import { syncAugmentationsToFile } from "~/lib/schema";

/**
 * POST /api/schema/computed-columns
 * Create a computed column
 */
export async function POST(request: NextRequest) {
  try {
    const body = (await request.json()) as {
      tableId: string;
      name: string;
      ast: unknown;
      type?: string;
      unit?: string | null;
    };
    const { tableId, name, ast, type, unit } = body;

    // Get table to find its name
    const table = await prisma.table.findUnique({
      where: { id: tableId },
      select: { name: true },
    });

    if (!table) {
      return NextResponse.json({ error: "Table not found" }, { status: 404 });
    }

    const computedColumn = await prisma.computedColumn.create({
      data: {
        name,
        ast: JSON.stringify(ast),
        type: type ?? "number", // Computed columns are arithmetic expressions, so always numeric
        unit: unit ?? null,
        selected: true,
        tableId,
      },
    });

    // Sync augmentations to file
    await syncAugmentationsToFile();

    return NextResponse.json({ computedColumn });
  } catch (error) {
    console.error("Failed to create computed column:", error);
    return NextResponse.json(
      {
        error:
          error instanceof Error
            ? error.message
            : "Failed to create computed column",
      },
      { status: 500 },
    );
  }
}
