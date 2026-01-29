import type { NextConfig } from "next";
import * as dotenv from "dotenv";
import * as path from "path";

// Load .inconvo.env before anything else
// This ensures env vars are available for @repo/connect imports
dotenv.config({ path: path.join(process.cwd(), ".inconvo.env") });

const config: NextConfig = {
  transpilePackages: [
    "@repo/ui",
    "@repo/types",
    "@repo/agents",
    "@repo/connect",
  ],
  serverExternalPackages: [
    "pino",
    "pino-pretty",
    "thread-stream",
    "better-sqlite3",
    "@prisma/adapter-better-sqlite3",
  ],
};

export default config;
