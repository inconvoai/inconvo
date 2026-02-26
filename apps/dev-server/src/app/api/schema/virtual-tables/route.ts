import { NextResponse, type NextRequest } from "next/server";
import { prisma } from "~/lib/prisma";
import { syncAugmentationsToFile } from "~/lib/schema";

/**
 * GET /api/schema/virtual-tables
 * List all virtual tables with their config and columns.
 */
export async function GET() {
  try {
    const tables = await prisma.table.findMany({
      where: { source: "VIRTUAL" },
      include: {
        virtualTableConfig: true,
        columns: {
          orderBy: { name: "asc" },
        },
        outwardRelations: {
          include: {
            columnMappings: { orderBy: { position: "asc" } },
            targetTable: { select: { name: true } },
          },
          orderBy: { name: "asc" },
        },
        condition: {
          include: {
            column: { select: { name: true } },
            userContextField: { select: { key: true } },
          },
        },
        accessPolicy: {
          include: { userContextField: { select: { key: true } } },
        },
      },
      orderBy: { name: "asc" },
    });

    return NextResponse.json({ tables });
  } catch (error) {
    console.error("Failed to get virtual tables:", error);
    return NextResponse.json(
      {
        error:
          error instanceof Error
            ? error.message
            : "Failed to get virtual tables",
      },
      { status: 500 },
    );
  }
}

/**
 * POST /api/schema/virtual-tables
 * Create a new virtual table.
 *
 * Body: { name, sql, dialect, columns: [{ sourceName, name, type, selected?, unit?, notes? }] }
 */
export async function POST(request: NextRequest) {
  try {
    const body = (await request.json()) as {
      name?: string;
      sql?: string;
      dialect?: string;
      columns?: Array<{
        sourceName: string;
        name?: string;
        type: string;
        selected?: boolean;
        unit?: string | null;
        notes?: string | null;
      }>;
    };

    const { name, sql, dialect, columns } = body;

    if (!name || !sql || !dialect) {
      return NextResponse.json(
        { error: "name, sql, and dialect are required" },
        { status: 400 },
      );
    }

    const validDialects = [
      "postgresql",
      "redshift",
      "mysql",
      "mssql",
      "bigquery",
    ];
    if (!validDialects.includes(dialect)) {
      return NextResponse.json(
        { error: `dialect must be one of: ${validDialects.join(", ")}` },
        { status: 400 },
      );
    }

    // Check for name collision with existing tables
    const existing = await prisma.table.findUnique({ where: { name } });
    if (existing) {
      return NextResponse.json(
        { error: `A table named "${name}" already exists` },
        { status: 409 },
      );
    }

    const table = await prisma.table.create({
      data: {
        name,
        source: "VIRTUAL",
        access: "QUERYABLE",
        virtualTableConfig: {
          create: { dialect, sql },
        },
        columns: {
          create: (columns ?? []).map((col) => ({
            name: col.sourceName,
            rename: col.name !== col.sourceName ? col.name : null,
            type: col.type,
            selected: col.selected ?? true,
            unit: col.unit ?? null,
            notes: col.notes ?? null,
          })),
        },
      },
      include: {
        virtualTableConfig: true,
        columns: { orderBy: { name: "asc" } },
      },
    });

    await syncAugmentationsToFile();

    return NextResponse.json({ table }, { status: 201 });
  } catch (error) {
    console.error("Failed to create virtual table:", error);
    return NextResponse.json(
      {
        error:
          error instanceof Error
            ? error.message
            : "Failed to create virtual table",
      },
      { status: 500 },
    );
  }
}
