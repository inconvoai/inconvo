import { type NextRequest, NextResponse } from "next/server";
import { getConversationWithMessages } from "~/lib/conversations";

// GET /api/conversations/[id] - Get a single conversation with messages
export async function GET(
  _request: NextRequest,
  { params }: { params: Promise<{ id: string }> },
) {
  const { id } = await params;

  const conversation = await getConversationWithMessages(id);

  if (!conversation) {
    return NextResponse.json(
      { error: "Conversation not found" },
      { status: 404 },
    );
  }

  return NextResponse.json({
    id: conversation.id,
    userIdentifier: conversation.userIdentifier,
    userContext: conversation.userContext ?? {},
    messages: conversation.messages,
  });
}
