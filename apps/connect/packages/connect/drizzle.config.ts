import "dotenv/config";
import { defineConfig } from "drizzle-kit";
import assert from "assert";

assert(process.env.INCONVO_DATABASE_URL, "INCONVO_DATABASE_URL is not set");

export default defineConfig({
  schema: "./drizzle/schema",
  dialect: "postgresql",
  dbCredentials: {
    url: process.env.INCONVO_DATABASE_URL,
  },
  introspect: {
    casing: "preserve",
  },
});
