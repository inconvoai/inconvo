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
    "better-sqlite3",
    "@prisma/adapter-better-sqlite3",
  ],
};

export default config;
