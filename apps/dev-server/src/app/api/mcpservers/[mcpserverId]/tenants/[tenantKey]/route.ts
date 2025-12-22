import { NextResponse } from "next/server";

// DELETE /api/mcpservers/[mcpserverId]/tenants/[tenantKey] - Delete tenant
export async function DELETE() {
  return NextResponse.json(
    { error: "Not implemented in dev server" },
    { status: 501 },
  );
}
