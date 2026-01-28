import { type NextRequest, NextResponse } from "next/server";
import {
  createConversation,
  listConversations,
  getConversation,
  deleteConversation,
} from "~/lib/conversations";

// GET /api/conversations - List all conversations
export async function GET() {
  const conversations = listConversations();
  return NextResponse.json({ conversations });
}

// POST /api/conversations - Create a new conversation
export async function POST(request: NextRequest) {
  try {
    const body = (await request.json().catch(() => ({}))) as {
      userIdentifier?: string;
      userContext?: Record<string, string | number>;
    };
    const userIdentifier = body.userIdentifier ?? "dev-user";
    const userContext = body.userContext ?? null;

    const conversation = createConversation(userIdentifier, userContext);
    return NextResponse.json(conversation, { status: 201 });
  } catch (error) {
    console.error(
      "[/api/conversations POST] Failed to create conversation:",
      error,
    );
    return NextResponse.json(
      { error: "Failed to create conversation" },
      { status: 500 },
    );
  }
}

// DELETE /api/conversations - Delete a conversation (expects ?id=xxx query param)
export async function DELETE(request: NextRequest) {
  const { searchParams } = new URL(request.url);
  const id = searchParams.get("id");

  if (!id) {
    return NextResponse.json(
      { error: "Missing conversation id" },
      { status: 400 },
    );
  }

  const existing = getConversation(id);
  if (!existing) {
    return NextResponse.json(
      { error: "Conversation not found" },
      { status: 404 },
    );
  }

  deleteConversation(id);
  return NextResponse.json({ success: true });
}
