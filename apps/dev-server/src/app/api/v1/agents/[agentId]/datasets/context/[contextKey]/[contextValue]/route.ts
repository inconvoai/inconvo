import { handleOptions } from "~/lib/cors";
import { notImplementedResponse } from "~/lib/notImplemented";

export async function OPTIONS() {
  return handleOptions();
}

export async function GET() {
  return notImplementedResponse("GET /api/v1/agents/{agentId}/datasets/context/{contextKey}/{contextValue}");
}

export async function POST() {
  return notImplementedResponse("POST /api/v1/agents/{agentId}/datasets/context/{contextKey}/{contextValue}");
}
