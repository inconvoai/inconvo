import { type NextRequest, NextResponse } from "next/server";
import { prisma } from "~/lib/prisma";

/**
 * GET /api/schema/request-context
 * List all request context fields
 */
export async function GET() {
  try {
    const fields = await prisma.requestContextField.findMany({
      include: {
        tableConditions: {
          include: {
            table: true,
            column: true,
          },
        },
      },
      orderBy: { key: "asc" },
    });

    return NextResponse.json({ fields });
  } catch (error) {
    console.error("Failed to get request context fields:", error);
    return NextResponse.json(
      {
        error: error instanceof Error ? error.message : "Failed to get fields",
      },
      { status: 500 },
    );
  }
}

/**
 * POST /api/schema/request-context
 * Create a new request context field
 */
export async function POST(request: NextRequest) {
  try {
    const body = (await request.json()) as { key?: string; type?: string };
    const { key, type } = body;

    if (!key || !type) {
      return NextResponse.json(
        { error: "key and type are required" },
        { status: 400 },
      );
    }

    if (!["STRING", "NUMBER"].includes(type)) {
      return NextResponse.json(
        { error: "type must be STRING or NUMBER" },
        { status: 400 },
      );
    }

    const field = await prisma.requestContextField.create({
      data: { key, type },
    });

    return NextResponse.json({ field }, { status: 201 });
  } catch (error) {
    console.error("Failed to create request context field:", error);
    return NextResponse.json(
      {
        error:
          error instanceof Error ? error.message : "Failed to create field",
      },
      { status: 500 },
    );
  }
}
