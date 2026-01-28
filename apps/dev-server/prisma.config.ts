import "dotenv/config";
import path from "path";
import { defineConfig } from "prisma/config";

// Allow overriding the database path via env var (used by CLI to persist across versions)
const dbPath =
  process.env.INCONVO_LOCAL_DB_PATH ||
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
