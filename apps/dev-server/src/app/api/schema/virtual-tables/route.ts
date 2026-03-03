import { NextResponse, type NextRequest } from "next/server";
import { prisma } from "~/lib/prisma";
import { syncAugmentationsToFile } from "~/lib/schema";
import {
  getDatabaseDialectErrorMessage,
  isDatabaseDialect,
  validateVirtualTableSql,
} from "~/lib/virtualTableValidation";

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
 * Body: { name, sql, dialect?, access? }
 */
export async function POST(request: NextRequest) {
  try {
    const body = (await request.json()) as {
      name?: string;
      sql?: string;
      dialect?: string;
      access?: "QUERYABLE" | "JOINABLE" | "OFF";
    };

    const { name, sql, dialect, access } = body;

    if (!name || !sql) {
      return NextResponse.json(
        { error: "name and sql are required" },
        { status: 400 },
      );
    }

    const validAccess = ["QUERYABLE", "JOINABLE", "OFF"] as const;
    if (dialect && !isDatabaseDialect(dialect)) {
      return NextResponse.json(
        { error: getDatabaseDialectErrorMessage() },
        { status: 400 },
      );
    }
    if (access && !validAccess.includes(access)) {
      return NextResponse.json(
        { error: `access must be one of: ${validAccess.join(", ")}` },
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

    const { dialect: validatedDialect, result } = await validateVirtualTableSql({
      sql,
      requestDialect: dialect,
      previewLimit: 5,
    });
    if (!result.ok) {
      return NextResponse.json({ error: result.error.message }, { status: 400 });
    }
    if (result.columns.length === 0) {
      return NextResponse.json(
        {
          error:
            "Validation returned no columns. The query may have produced zero preview rows; add a filter/limit for validation or use data that returns at least one row.",
        },
        { status: 400 },
      );
    }

    const table = await prisma.$transaction(async (tx) => {
      return await tx.table.create({
        data: {
          name,
          source: "VIRTUAL",
          access: access ?? "OFF",
          virtualTableConfig: {
            create: { dialect: validatedDialect, sql },
          },
          columns: {
            create: result.columns.map((column) => ({
              name: column.sourceName,
              rename: null,
              type: column.type,
              selected: true,
              unit: null,
              notes: null,
            })),
          },
        },
        include: {
          virtualTableConfig: true,
          columns: { orderBy: { name: "asc" } },
        },
      });
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
