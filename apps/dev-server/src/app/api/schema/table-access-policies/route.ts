import { type NextRequest, NextResponse } from "next/server";
import { prisma } from "~/lib/prisma";

/**
 * POST /api/schema/table-access-policies
 * Create or update a table access policy.
 */
export async function POST(request: NextRequest) {
  try {
    const body = (await request.json()) as {
      tableId?: string;
      userContextFieldId?: string;
    };
    const { tableId, userContextFieldId } = body;

    if (!tableId || !userContextFieldId) {
      return NextResponse.json(
        { error: "tableId and userContextFieldId are required" },
        { status: 400 },
      );
    }

    const [table, field] = await Promise.all([
      prisma.table.findUnique({
        where: { id: tableId },
        select: { id: true },
      }),
      prisma.userContextField.findUnique({
        where: { id: userContextFieldId },
        select: {
          id: true,
          type: true,
        },
      }),
    ]);

    if (!table) {
      return NextResponse.json({ error: "Table not found" }, { status: 404 });
    }
    if (!field) {
      return NextResponse.json(
        { error: "User context field not found" },
        { status: 404 },
      );
    }
    if (field.type !== "BOOLEAN") {
      return NextResponse.json(
        { error: "Table access policy requires a BOOLEAN user context field" },
        { status: 400 },
      );
    }

    const policy = await prisma.tableAccessPolicy.upsert({
      where: { tableId },
      create: {
        tableId,
        userContextFieldId,
      },
      update: {
        userContextFieldId,
      },
      include: {
        userContextField: true,
      },
    });

    return NextResponse.json({ policy });
  } catch (error) {
    console.error("Failed to upsert table access policy:", error);
    return NextResponse.json(
      {
        error:
          error instanceof Error
            ? error.message
            : "Failed to upsert table access policy",
      },
      { status: 500 },
    );
  }
}

/**
 * DELETE /api/schema/table-access-policies
 * Delete a table access policy.
 */
export async function DELETE(request: NextRequest) {
  try {
    const body = (await request.json()) as { tableId?: string };
    const { tableId } = body;

    if (!tableId) {
      return NextResponse.json(
        { error: "tableId is required" },
        { status: 400 },
      );
    }

    const existing = await prisma.tableAccessPolicy.findUnique({
      where: { tableId },
      select: { id: true },
    });

    if (!existing) {
      return NextResponse.json(
        { error: "Table access policy not found" },
        { status: 404 },
      );
    }

    await prisma.tableAccessPolicy.delete({
      where: { tableId },
    });

    return NextResponse.json({ success: true });
  } catch (error) {
    console.error("Failed to delete table access policy:", error);
    return NextResponse.json(
      {
        error:
          error instanceof Error
            ? error.message
            : "Failed to delete table access policy",
      },
      { status: 500 },
    );
  }
}
