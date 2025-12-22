import { NextResponse } from "next/server";

// POST /api/conversations/[id]/response/[responseId]/feedback - Create feedback
export async function POST() {
  return NextResponse.json(
    { error: "Not implemented in dev server" },
    { status: 501 },
  );
}
