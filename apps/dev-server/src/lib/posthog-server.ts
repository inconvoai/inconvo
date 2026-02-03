import { PostHog } from "posthog-node";

let posthogClient: PostHog | null = null;

export function getPostHogClient() {
  // Check if telemetry is disabled
  if (process.env.DISABLE_TELEMETRY === "true") {
    return null;
  }

  try {
    posthogClient ??= new PostHog(
      "phc_bZkkUMPxJpRbeUNwsF2muSvwtUUgIKn9MOce9kDoKBY",
      {
        host: "https://eu.i.posthog.com",
      flushAt: 1,
      flushInterval: 0,
    });
  } catch (_error) {
    // Silent failure - return null
    return null;
  }

  return posthogClient;
}

export async function shutdownPostHog() {
  if (posthogClient) {
    await posthogClient.shutdown();
  }
}
