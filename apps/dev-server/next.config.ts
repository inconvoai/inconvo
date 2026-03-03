import type { NextConfig } from "next";
import * as dotenv from "dotenv";
import * as path from "path";

// Load .inconvo.env before anything else
// This ensures env vars are available for @repo/connect imports
dotenv.config({ path: path.join(process.cwd(), ".inconvo.env") });

const config: NextConfig = {
  output: "standalone",
  transpilePackages: [
    "@repo/ui",
    "@repo/types",
    "@repo/agents",
    "@repo/connect",
  ],
  serverExternalPackages: [
    "pino",
    "pino-pretty",
    "pino-std-serializers",
    "thread-stream",
    "pg",
    "pg-types",
  ],
  // PostHog reverse proxy configuration
  async rewrites() {
    // Skip PostHog proxy if telemetry is disabled
    if (process.env.DISABLE_TELEMETRY === "true") {
      return [];
    }

    return [
      {
        source: "/ingest/static/:path*",
        destination: "https://eu-assets.i.posthog.com/static/:path*",
      },
      {
        source: "/ingest/:path*",
        destination: "https://eu.i.posthog.com/:path*",
      },
    ];
  },
  // Required to support PostHog trailing slash API requests
  skipTrailingSlashRedirect: true,
};

export default config;
