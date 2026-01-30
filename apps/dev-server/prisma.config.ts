import path from "path";
import { defineConfig } from "prisma/config";

// Try to load dotenv (not available in production Docker image)
try {
  // eslint-disable-next-line @typescript-eslint/no-require-imports
  require("dotenv/config");
} catch {
  // dotenv not available, env vars should already be set
}

// Allow overriding the database path via env var (used by CLI to persist across versions)
const dbPath =
  process.env.INCONVO_LOCAL_DB_PATH ??
  path.join(process.cwd(), "prisma", ".inconvo.db");

export default defineConfig({
  schema: "prisma/schema.prisma",
  migrations: {
    path: "prisma/migrations",
  },
  datasource: {
    url: `file:${dbPath}`,
  },
});
