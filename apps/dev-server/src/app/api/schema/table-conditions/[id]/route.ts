import { type NextRequest, NextResponse } from "next/server";
import { prisma } from "~/lib/prisma";

interface RouteParams {
  params: Promise<{ id: string }>;
}

/**
 * DELETE /api/schema/table-conditions/[id]
 * Delete a table condition
 */
export async function DELETE(request: NextRequest, { params }: RouteParams) {
  const { id } = await params;

  try {
    await prisma.tableCondition.delete({
      where: { id },
    });

    return NextResponse.json({ success: true });
  } catch (error) {
    console.error("Failed to delete table condition:", error);
    return NextResponse.json(
      {
        error:
          error instanceof Error ? error.message : "Failed to delete condition",
      },
      { status: 500 },
    );
  }
}
