import "dotenv/config";
import { defineConfig } from "drizzle-kit";

export default defineConfig({
  schema: "./drizzle/schema",
  dialect: "postgresql",
  dbCredentials: {
    url: "postgresql://metabase:metasample123@localhost:2468/sample",
  },
});
