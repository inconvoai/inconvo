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
        requestContextField: true,
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
      requestContextFieldId?: string;
    };
    const { tableId, columnId, requestContextFieldId } = body;

    if (!tableId || !columnId || !requestContextFieldId) {
      return NextResponse.json(
        { error: "tableId, columnId, and requestContextFieldId are required" },
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
        data: { columnId, requestContextFieldId },
        include: {
          table: true,
          column: true,
          requestContextField: true,
        },
      });
      return NextResponse.json({ condition });
    }

    const condition = await prisma.tableCondition.create({
      data: { tableId, columnId, requestContextFieldId },
      include: {
        table: true,
        column: true,
        requestContextField: true,
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
