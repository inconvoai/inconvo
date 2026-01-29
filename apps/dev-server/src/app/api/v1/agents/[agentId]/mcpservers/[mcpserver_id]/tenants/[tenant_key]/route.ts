import { handleOptions } from "~/lib/cors";
import { notImplementedResponse } from "~/lib/notImplemented";

export async function OPTIONS() {
  return handleOptions();
}

export async function DELETE() {
  return notImplementedResponse("DELETE /api/v1/agents/{agentId}/mcpservers/{mcpserver_id}/tenants/{tenant_key}");
}
