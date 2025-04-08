import { defineConfig } from "drizzle-kit";
import { env } from "~/env";

export default defineConfig({
  schema: "./drizzle/schema",
  dialect: env.DATABASE_DIALECT,
  dbCredentials: {
    url: env.INCONVO_DATABASE_URL,
  },
  introspect: {
    casing: "preserve",
  },
});
