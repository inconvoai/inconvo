import { type NextRequest, NextResponse } from "next/server";
import { getConversation } from "~/lib/conversations";

// GET /api/conversations/[id] - Get a single conversation
export async function GET(
  _request: NextRequest,
  { params }: { params: Promise<{ id: string }> },
) {
  const { id } = await params;

  const conversation = getConversation(id);

  if (!conversation) {
    return NextResponse.json(
      { error: "Conversation not found" },
      { status: 404 },
    );
  }

  // Return in SDK-compatible format
  return NextResponse.json({
    id: conversation.id,
    userIdentifier: conversation.userIdentifier,
    userContext: conversation.userContext ?? {},
    messages: [], // Messages are not persisted in the dev server's in-memory store
  });
}
