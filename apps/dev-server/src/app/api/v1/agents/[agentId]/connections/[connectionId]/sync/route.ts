import { type NextRequest, NextResponse } from "next/server";
import { syncSchema } from "~/lib/schema";
import { corsHeaders, handleOptions } from "~/lib/cors";
import { DEV_AGENT_ID } from "~/lib/constants";

// Handle OPTIONS preflight
export async function OPTIONS() {
  return handleOptions();
}

// POST /api/v1/agents/{agentId}/connections/{connectionId}/sync - Sync connection schema
export async function POST(
  _request: NextRequest,
  { params }: { params: Promise<{ agentId: string; connectionId: string }> },
) {
  try {
    const { agentId } = await params;

    if (agentId !== DEV_AGENT_ID) {
      return NextResponse.json(
        { error: "Invalid agent ID" },
        { status: 404, headers: corsHeaders },
      );
    }

    await syncSchema();

    return NextResponse.json(
      { message: "sync complete" },
      { status: 200, headers: corsHeaders },
    );
  } catch (error) {
    console.error(
      "[POST /api/v1/agents/[agentId]/connections/[connectionId]/sync] Error:",
      error,
    );
    return NextResponse.json(
      {
        error: error instanceof Error ? error.message : "Failed to sync schema",
      },
      { status: 500, headers: corsHeaders },
    );
  }
}
