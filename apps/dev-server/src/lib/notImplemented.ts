import { NextResponse } from "next/server";
import { corsHeaders } from "./cors";

/**
 * Standard 501 response for cloud-only endpoints
 */
export function notImplementedResponse(endpoint: string) {
  return NextResponse.json(
    {
      error: "Not implemented in OSS dev-server",
      message: `The endpoint '${endpoint}' is only available in Inconvo Cloud`,
    },
    {
      status: 501,
      headers: corsHeaders,
    },
  );
}
