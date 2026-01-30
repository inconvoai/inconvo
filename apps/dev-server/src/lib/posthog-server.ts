import { PostHog } from "posthog-node";

let posthogClient: PostHog | null = null;

export function getPostHogClient() {
  // Check if telemetry is disabled
  if (process.env.DISABLE_TELEMETRY === "true") {
    return null;
  }

  // Return null if no key is configured
  if (!process.env.NEXT_PUBLIC_POSTHOG_KEY) {
    return null;
  }

  try {
    posthogClient ??= new PostHog(process.env.NEXT_PUBLIC_POSTHOG_KEY, {
      host: process.env.NEXT_PUBLIC_POSTHOG_HOST,
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
