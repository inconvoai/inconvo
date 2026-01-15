import { type NextRequest, NextResponse } from "next/server";
import { prisma } from "~/lib/prisma";

interface RouteParams {
  params: Promise<{ name: string }>;
}

/**
 * GET /api/schema/tables/[name]
 * Get a single table with full configuration
 */
export async function GET(request: NextRequest, { params }: RouteParams) {
  const { name } = await params;

  try {
    const table = await prisma.table.findUnique({
      where: { name },
      include: {
        columns: {
          include: {
            conversion: true,
          },
          orderBy: { name: "asc" },
        },
        computedColumns: {
          orderBy: { name: "asc" },
        },
        outwardRelations: {
          include: {
            columnMappings: true,
            targetTable: true,
          },
        },
        condition: {
          include: {
            column: true,
            userContextField: true,
          },
        },
      },
    });

    if (!table) {
      return NextResponse.json({ error: "Table not found" }, { status: 404 });
    }

    return NextResponse.json({ table });
  } catch (error) {
    console.error("Failed to get table:", error);
    return NextResponse.json(
      { error: error instanceof Error ? error.message : "Failed to get table" },
      { status: 500 },
    );
  }
}

/**
 * PATCH /api/schema/tables/[name]
 * Update table settings (access, context)
 */
export async function PATCH(request: NextRequest, { params }: RouteParams) {
  const { name } = await params;

  try {
    const body = (await request.json()) as {
      access?: string;
      context?: string;
    };
    const { access, context } = body;

    const table = await prisma.table.update({
      where: { name },
      data: {
        ...(access !== undefined && { access }),
        ...(context !== undefined && { context }),
      },
    });

    return NextResponse.json({ table });
  } catch (error) {
    console.error("Failed to update table:", error);
    return NextResponse.json(
      {
        error:
          error instanceof Error ? error.message : "Failed to update table",
      },
      { status: 500 },
    );
  }
}
