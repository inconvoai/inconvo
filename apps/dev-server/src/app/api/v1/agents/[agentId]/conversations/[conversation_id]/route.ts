import { type NextRequest, NextResponse } from "next/server";
import {
  getConversationWithMessages,
  deleteConversation,
} from "~/lib/conversations";
import { corsHeaders, handleOptions } from "~/lib/cors";
import { DEV_AGENT_ID } from "~/lib/constants";

// Handle OPTIONS preflight
export async function OPTIONS() {
  return handleOptions();
}

// GET /api/v1/agents/{agentId}/conversations/{conversation_id} - Get a conversation
export async function GET(
  _request: NextRequest,
  { params }: { params: Promise<{ agentId: string; conversation_id: string }> },
) {
  try {
    const { agentId, conversation_id } = await params;

    // Validate agent ID
    if (agentId !== DEV_AGENT_ID) {
      return NextResponse.json(
        { error: "Invalid agent ID" },
        { status: 404, headers: corsHeaders },
      );
    }

    // Get conversation with messages
    const conversation = await getConversationWithMessages(conversation_id);

    if (!conversation) {
      return NextResponse.json(
        { error: "Conversation not found" },
        { status: 404, headers: corsHeaders },
      );
    }

    return NextResponse.json(conversation, { headers: corsHeaders });
  } catch (error) {
    console.error(
      "[GET /api/v1/agents/[agentId]/conversations/[conversation_id]] Error:",
      error,
    );
    return NextResponse.json(
      {
        error:
          error instanceof Error ? error.message : "Failed to get conversation",
      },
      { status: 500, headers: corsHeaders },
    );
  }
}

// DELETE /api/v1/agents/{agentId}/conversations/{conversation_id} - Delete a conversation
export async function DELETE(
  _request: NextRequest,
  { params }: { params: Promise<{ agentId: string; conversation_id: string }> },
) {
  try {
    const { agentId, conversation_id } = await params;

    // Validate agent ID
    if (agentId !== DEV_AGENT_ID) {
      return NextResponse.json(
        { error: "Invalid agent ID" },
        { status: 404, headers: corsHeaders },
      );
    }

    // Delete conversation
    const success = await deleteConversation(conversation_id);

    if (!success) {
      return NextResponse.json(
        { error: "Conversation not found" },
        { status: 404, headers: corsHeaders },
      );
    }

    return NextResponse.json(
      { success: true },
      { status: 200, headers: corsHeaders },
    );
  } catch (error) {
    console.error(
      "[DELETE /api/v1/agents/[agentId]/conversations/[conversation_id]] Error:",
      error,
    );
    return NextResponse.json(
      {
        error:
          error instanceof Error
            ? error.message
            : "Failed to delete conversation",
      },
      { status: 500, headers: corsHeaders },
    );
  }
}
