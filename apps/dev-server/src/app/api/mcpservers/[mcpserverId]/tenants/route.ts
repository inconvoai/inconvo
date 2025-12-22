import { NextResponse } from "next/server";

// POST /api/mcpservers/[mcpserverId]/tenants - Create tenant
export async function POST() {
  return NextResponse.json(
    { error: "Not implemented in dev server" },
    { status: 501 },
  );
}
