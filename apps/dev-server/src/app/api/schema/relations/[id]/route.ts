import { type NextRequest, NextResponse } from "next/server";
import { prisma } from "~/lib/prisma";

interface RouteParams {
  params: Promise<{ id: string }>;
}

/**
 * PATCH /api/schema/relations/[id]
 * Update relation settings (selected)
 */
export async function PATCH(request: NextRequest, { params }: RouteParams) {
  const { id } = await params;

  try {
    const body = (await request.json()) as {
      selected?: boolean;
    };
    const { selected } = body;

    const relation = await prisma.relation.update({
      where: { id },
      data: {
        ...(selected !== undefined && { selected }),
      },
    });

    return NextResponse.json({ relation });
  } catch (error) {
    console.error("Failed to update relation:", error);
    return NextResponse.json(
      {
        error:
          error instanceof Error ? error.message : "Failed to update relation",
      },
      { status: 500 },
    );
  }
}
