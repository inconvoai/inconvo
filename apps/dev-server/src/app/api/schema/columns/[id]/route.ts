import { type NextRequest, NextResponse } from "next/server";
import { prisma } from "~/lib/prisma";

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
      rename?: string;
      notes?: string;
      unit?: string;
      selected?: boolean;
    };
    const { rename, notes, unit, selected } = body;

    const column = await prisma.column.update({
      where: { id },
      data: {
        ...(rename !== undefined && { rename }),
        ...(notes !== undefined && { notes }),
        ...(unit !== undefined && { unit }),
        ...(selected !== undefined && { selected }),
      },
    });

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
