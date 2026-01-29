import { NextResponse } from "next/server";

// PATCH /api/conversations/[id]/response/[responseId]/feedback/[feedbackId] - Update feedback
export async function PATCH() {
  return NextResponse.json(
    { error: "Not implemented in dev server" },
    { status: 501 },
  );
}
