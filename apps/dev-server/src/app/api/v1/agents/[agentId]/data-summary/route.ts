import { type NextRequest, NextResponse } from "next/server";
import { getSchema } from "~/lib/schema";
import { corsHeaders, handleOptions } from "~/lib/cors";
import { DEV_AGENT_ID } from "~/lib/constants";

// Handle OPTIONS preflight
export async function OPTIONS() {
  return handleOptions();
}

// GET /api/v1/agents/{agentId}/data-summary - Get database schema summary
export async function GET(
  _request: NextRequest,
  { params }: { params: Promise<{ agentId: string }> },
) {
  try {
    const { agentId } = await params;

    // Validate agent ID
    if (agentId !== DEV_AGENT_ID) {
      return NextResponse.json(
        { error: "Invalid agent ID" },
        { status: 404, headers: corsHeaders },
      );
    }

    // Get schema
    const schema = await getSchema();

    return NextResponse.json(schema, { headers: corsHeaders });
  } catch (error) {
    console.error("[GET /api/v1/agents/[agentId]/data-summary] Error:", error);
    return NextResponse.json(
      {
        error:
          error instanceof Error ? error.message : "Failed to get data summary",
      },
      { status: 500, headers: corsHeaders },
    );
  }
}
