import { NextResponse } from "next/server";

// GET /api/conversations/[id]/response/[responseId] - Get a response
export async function GET() {
  return NextResponse.json(
    { error: "Not implemented in dev server" },
    { status: 501 },
  );
}
