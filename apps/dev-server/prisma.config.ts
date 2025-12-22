import "dotenv/config";
import path from "path";
import { defineConfig } from "prisma/config";

export default defineConfig({
  schema: "prisma/schema.prisma",
  migrations: {
    path: "prisma/migrations",
  },
  datasource: {
    // SQLite database file path
    url: `file:${path.join(process.cwd(), "prisma", ".inconvo.db")}`,
  },
});
