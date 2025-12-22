import { NextResponse } from "next/server";

// GET /api/agents/data-summary - Get data summary
export async function GET() {
  return NextResponse.json(
    { error: "Not implemented in dev server" },
    { status: 501 },
  );
}
