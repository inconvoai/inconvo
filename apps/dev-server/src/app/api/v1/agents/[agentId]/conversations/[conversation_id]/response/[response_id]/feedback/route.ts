import { handleOptions } from "~/lib/cors";
import { notImplementedResponse } from "~/lib/notImplemented";

export async function OPTIONS() {
  return handleOptions();
}

export async function POST() {
  return notImplementedResponse("POST /api/v1/agents/{agentId}/conversations/{conversation_id}/response/{response_id}/feedback");
}
