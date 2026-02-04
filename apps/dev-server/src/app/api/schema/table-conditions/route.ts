import { type NextRequest, NextResponse } from "next/server";
import { prisma } from "~/lib/prisma";

/**
 * GET /api/schema/table-conditions
 * List all table conditions
 */
export async function GET() {
  try {
    const conditions = await prisma.tableCondition.findMany({
      include: {
        table: true,
        column: true,
        userContextField: true,
      },
    });

    return NextResponse.json({ conditions });
  } catch (error) {
    console.error("Failed to get table conditions:", error);
    return NextResponse.json(
      {
        error:
          error instanceof Error ? error.message : "Failed to get conditions",
      },
      { status: 500 },
    );
  }
}

/**
 * POST /api/schema/table-conditions
 * Create a table condition (row-level security)
 */
export async function POST(request: NextRequest) {
  try {
    const body = (await request.json()) as {
      tableId?: string;
      columnId?: string;
      userContextFieldId?: string;
    };
    const { tableId, columnId, userContextFieldId } = body;

    if (!tableId || !columnId || !userContextFieldId) {
      return NextResponse.json(
        { error: "tableId, columnId, and userContextFieldId are required" },
        { status: 400 },
      );
    }

    // Check if table already has a condition
    const existingCondition = await prisma.tableCondition.findUnique({
      where: { tableId },
    });

    if (existingCondition) {
      // Update existing condition
      const condition = await prisma.tableCondition.update({
        where: { tableId },
        data: { columnId, userContextFieldId },
        include: {
          table: true,
          column: true,
          userContextField: true,
        },
      });
      return NextResponse.json({ condition });
    }

    const condition = await prisma.tableCondition.create({
      data: { tableId, columnId, userContextFieldId },
      include: {
        table: true,
        column: true,
        userContextField: true,
      },
    });

    return NextResponse.json({ condition }, { status: 201 });
  } catch (error) {
    console.error("Failed to create table condition:", error);
    return NextResponse.json(
      {
        error:
          error instanceof Error ? error.message : "Failed to create condition",
      },
      { status: 500 },
    );
  }
}

/**
 * DELETE /api/schema/table-conditions
 * Delete a table condition (row-level security)
 */
export async function DELETE(request: NextRequest) {
  try {
    const body = (await request.json()) as {
      tableId?: string;
    };
    const { tableId } = body;

    if (!tableId) {
      return NextResponse.json(
        { error: "tableId is required" },
        { status: 400 },
      );
    }

    // Check if condition exists
    const existingCondition = await prisma.tableCondition.findUnique({
      where: { tableId },
    });

    if (!existingCondition) {
      return NextResponse.json(
        { error: "Table condition not found" },
        { status: 404 },
      );
    }

    // Delete the condition
    await prisma.tableCondition.delete({
      where: { tableId },
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
