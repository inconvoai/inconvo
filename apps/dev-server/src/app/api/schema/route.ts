import { NextResponse } from "next/server";
import { getSchema, syncSchema } from "~/lib/schema";

/**
 * GET /api/schema
 * Returns the full merged schema for the agent
 */
export async function GET() {
  try {
    const schema = await getSchema();
    return NextResponse.json({ schema });
  } catch (error) {
    console.error("Failed to get schema:", error);
    return NextResponse.json(
      {
        error: error instanceof Error ? error.message : "Failed to get schema",
      },
      { status: 500 },
    );
  }
}

/**
 * POST /api/schema
 * Syncs the schema from the database
 */
export async function POST() {
  try {
    const result = await syncSchema();
    return NextResponse.json({
      success: true,
      added: result.added,
      updated: result.updated,
    });
  } catch (error) {
    console.error("Failed to sync schema:", error);
    return NextResponse.json(
      {
        error: error instanceof Error ? error.message : "Failed to sync schema",
      },
      { status: 500 },
    );
  }
}
