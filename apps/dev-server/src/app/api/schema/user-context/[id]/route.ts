import { type NextRequest, NextResponse } from "next/server";
import { prisma } from "~/lib/prisma";

interface RouteParams {
  params: Promise<{ id: string }>;
}

/**
 * PATCH /api/schema/user-context/[id]
 * Update a user context field
 */
export async function PATCH(request: NextRequest, { params }: RouteParams) {
  const { id } = await params;

  try {
    const body = (await request.json()) as { key?: string; type?: string };
    const { key, type } = body;

    const field = await prisma.userContextField.update({
      where: { id },
      data: {
        ...(key !== undefined && { key }),
        ...(type !== undefined && { type }),
      },
    });

    return NextResponse.json({ field });
  } catch (error) {
    console.error("Failed to update user context field:", error);
    return NextResponse.json(
      {
        error:
          error instanceof Error ? error.message : "Failed to update field",
      },
      { status: 500 },
    );
  }
}

/**
 * DELETE /api/schema/user-context/[id]
 * Delete a user context field
 */
export async function DELETE(request: NextRequest, { params }: RouteParams) {
  const { id } = await params;

  try {
    await prisma.userContextField.delete({
      where: { id },
    });

    return NextResponse.json({ success: true });
  } catch (error) {
    console.error("Failed to delete user context field:", error);
    return NextResponse.json(
      {
        error:
          error instanceof Error ? error.message : "Failed to delete field",
      },
      { status: 500 },
    );
  }
}
