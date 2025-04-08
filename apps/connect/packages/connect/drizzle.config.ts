import { defineConfig } from "drizzle-kit";
import { env } from "~/env";
import path from "path";

export default defineConfig({
  out: path.resolve(__dirname, "../drizzle"),
  dialect: env.DATABASE_DIALECT,
  dbCredentials: {
    url: env.INCONVO_DATABASE_URL,
  },
  introspect: {
    casing: "preserve",
  },
});
