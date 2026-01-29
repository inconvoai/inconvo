import { type NextRequest, NextResponse } from "next/server";
import {
  createConversation,
  listConversationsFiltered,
} from "~/lib/conversations";
import { corsHeaders, handleOptions } from "~/lib/cors";
import { parseListParams, decodeCursor } from "~/lib/apiParams";
import { DEV_AGENT_ID } from "~/lib/constants";

// Handle OPTIONS preflight
export async function OPTIONS() {
  return handleOptions();
}

// POST /api/v1/agents/{agentId}/conversations - Create a conversation
export async function POST(
  request: NextRequest,
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

    // Parse request body
    const body = await request.json() as {
      userIdentifier?: string;
      userContext?: Record<string, string | number>;
    };

    const { userIdentifier, userContext } = body;

    if (!userIdentifier || typeof userIdentifier !== "string") {
      return NextResponse.json(
        { error: "userIdentifier is required and must be a string" },
        { status: 400, headers: corsHeaders },
      );
    }

    // Create conversation
    const conversation = await createConversation(userIdentifier, userContext);

    return NextResponse.json(conversation, {
      status: 201,
      headers: corsHeaders,
    });
  } catch (error) {
    console.error("[POST /api/v1/agents/[agentId]/conversations] Error:", error);
    return NextResponse.json(
      {
        error: error instanceof Error ? error.message : "Failed to create conversation",
      },
      { status: 500, headers: corsHeaders },
    );
  }
}

// GET /api/v1/agents/{agentId}/conversations - List conversations
export async function GET(
  request: NextRequest,
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

    // Parse query parameters
    const searchParams = request.nextUrl.searchParams;
    const { cursor, limit, userIdentifier, userContext } = parseListParams(searchParams);

    // Decode cursor if provided
    let decodedCursor: Date | undefined = undefined;
    if (cursor) {
      const decoded = decodeCursor(cursor);
      if (!decoded) {
        return NextResponse.json(
          { error: "Invalid cursor" },
          { status: 400, headers: corsHeaders },
        );
      }
      decodedCursor = decoded;
    }

    // List conversations with filters
    const result = await listConversationsFiltered({
      cursor: decodedCursor,
      limit,
      userIdentifier,
      userContext,
    });

    return NextResponse.json(result, { headers: corsHeaders });
  } catch (error) {
    console.error("[GET /api/v1/agents/[agentId]/conversations] Error:", error);
    return NextResponse.json(
      {
        error: error instanceof Error ? error.message : "Failed to list conversations",
      },
      { status: 500, headers: corsHeaders },
    );
  }
}
