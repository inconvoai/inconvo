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
        // Privacy settings - disable automatic PII collection
        person_profiles: "never", // Don't create person profiles
        disable_session_recording: true, // No session recordings
        autocapture: false, // No automatic event capture
        capture_pageview: false, // No automatic pageviews
        capture_pageleave: false, // No automatic pageleave events
        // Strip all PII properties before sending
        before_send: function (event) {
          // List of properties to strip
          const propsToStrip = [
            "$os",
            "$os_version",
            "$browser",
            "$browser_version",
            "$device_type",
            "$screen_height",
            "$screen_width",
            "$viewport_height",
            "$viewport_width",
            "$search_engine",
            "$referrer",
            "$referring_domain",
            "$ip",
            "$geoip_city_name",
            "$geoip_country_name",
            "$geoip_country_code",
            "$geoip_continent_name",
            "$geoip_continent_code",
            "$geoip_latitude",
            "$geoip_longitude",
            "$geoip_time_zone",
            "$geoip_subdivision_1_code",
            "$geoip_subdivision_1_name",
            "$geoip_subdivision_2_code",
            "$geoip_subdivision_2_name",
            "$geoip_subdivision_3_code",
            "$geoip_subdivision_3_name",
          ];

          // Strip each property
          if (event?.properties) {
            propsToStrip.forEach((prop) => {
              if (event.properties[prop]) {
                event.properties[prop] = null;
              }
            });
          }

          return event;
        },
        // Enables capturing unhandled exceptions via Error Tracking
        capture_exceptions: true,
        // Turn on debug in development mode
        debug: process.env.NODE_ENV === "development",
      });
    }
  } catch (_error) {
    // Silent failure - never impact app functionality
  }
}
