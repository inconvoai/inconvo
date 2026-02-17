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
            augmentation: {
              include: {
                conversionConfig: true,
                staticEnumConfig: true,
              },
            },
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
        accessPolicy: {
          include: {
            userContextField: true,
          },
        },
      },
    });

    if (!table) {
      return NextResponse.json({ error: "Table not found" }, { status: 404 });
    }

    const mappedTable = {
      ...table,
      columns: table.columns.map((column) => {
        const conversion =
          column.augmentation?.kind === "CONVERSION" &&
          column.augmentation.conversionConfig
            ? {
                id: column.augmentation.id,
                ast: (() => {
                  try {
                    return JSON.parse(
                      column.augmentation.conversionConfig.ast,
                    ) as unknown;
                  } catch {
                    return column.augmentation.conversionConfig.ast;
                  }
                })(),
                type: column.augmentation.conversionConfig.type ?? null,
                selected: column.augmentation.selected,
              }
            : null;

        const valueEnum =
          column.augmentation?.kind === "STATIC_ENUM" &&
          column.augmentation.staticEnumConfig
            ? {
                id: column.augmentation.id,
                selected: column.augmentation.selected,
                entries: column.augmentation.staticEnumConfig.entries,
              }
            : null;

        const { augmentation, ...rest } = column;
        return {
          ...rest,
          conversion,
          valueEnum,
        };
      }),
    };

    return NextResponse.json({ table: mappedTable });
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
