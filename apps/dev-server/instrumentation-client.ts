import posthog from "posthog-js";

// Check if telemetry is disabled
if (process.env.NEXT_PUBLIC_DISABLE_TELEMETRY === "true") {
  // Exit early - do not initialize PostHog
} else {
  try {
    // Only initialize if we have a valid key
    if (process.env.NEXT_PUBLIC_POSTHOG_KEY) {
      posthog.init(process.env.NEXT_PUBLIC_POSTHOG_KEY, {
        api_host: "/ingest",
        ui_host: process.env.NEXT_PUBLIC_POSTHOG_HOST,
        // Include the defaults option as required by PostHog
        defaults: "2025-11-30",
        // Enables capturing unhandled exceptions via Error Tracking
        capture_exceptions: true,
        // Turn on debug in development mode
        debug: process.env.NODE_ENV === "development",
      });
    }
  } catch (error) {
    // Silent failure - never impact app functionality
  }
}
