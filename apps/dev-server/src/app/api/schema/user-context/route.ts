import { type NextRequest, NextResponse } from "next/server";
import { prisma } from "~/lib/prisma";

/**
 * GET /api/schema/user-context
 * List all user context fields
 */
export async function GET() {
  try {
    const config = await prisma.userContextConfig.findFirst({
      select: { status: true },
    });
    const fields = await prisma.userContextField.findMany({
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

    return NextResponse.json({ fields, status: config?.status ?? "UNSET" });
  } catch (error) {
    console.error("Failed to get user context fields:", error);
    return NextResponse.json(
      {
        error: error instanceof Error ? error.message : "Failed to get fields",
      },
      { status: 500 },
    );
  }
}

/**
 * POST /api/schema/user-context
 * Create a new user context field
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

    if (!["STRING", "NUMBER", "BOOLEAN"].includes(type)) {
      return NextResponse.json(
        { error: "type must be STRING, NUMBER, or BOOLEAN" },
        { status: 400 },
      );
    }

    const config = await prisma.userContextConfig.findFirst({
      select: { id: true },
    });
    if (!config) {
      await prisma.userContextConfig.create({
        data: {},
      });
    }

    const field = await prisma.userContextField.create({
      data: { key, type },
    });

    return NextResponse.json({ field }, { status: 201 });
  } catch (error) {
    console.error("Failed to create user context field:", error);
    return NextResponse.json(
      {
        error:
          error instanceof Error ? error.message : "Failed to create field",
      },
      { status: 500 },
    );
  }
}

/**
 * PATCH /api/schema/user-context
 * Update user context status
 */
export async function PATCH(request: NextRequest) {
  try {
    const body = (await request.json()) as {
      status?: "ENABLED" | "DISABLED";
    };
    const { status } = body;

    if (!status || !["ENABLED", "DISABLED"].includes(status)) {
      return NextResponse.json(
        { error: "status must be ENABLED or DISABLED" },
        { status: 400 },
      );
    }

    const config = await prisma.userContextConfig.findFirst({
      select: { id: true },
    });

    const updated = config
      ? await prisma.userContextConfig.update({
          where: { id: config.id },
          data: { status },
        })
      : await prisma.userContextConfig.create({ data: { status } });

    return NextResponse.json({ status: updated.status });
  } catch (error) {
    console.error("Failed to update user context status:", error);
    return NextResponse.json(
      {
        error:
          error instanceof Error ? error.message : "Failed to update status",
      },
      { status: 500 },
    );
  }
}
