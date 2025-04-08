import { defineConfig } from "drizzle-kit";
import { env } from "~/env";

export default defineConfig({
  out: "./drizzle",
  dialect: env.DATABASE_DIALECT,
  dbCredentials: {
    url: env.INCONVO_DATABASE_URL,
  },
  introspect: {
    casing: "preserve",
  },
});
