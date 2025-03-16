import { createEnv } from "@t3-oss/env-core";
import { z } from "zod";
import "dotenv/config";

export const env = createEnv({
  server: {
    INCONVO_DATABASE_URL: z.string(),
    INCONVO_SECRET_KEY: z.string().min(1),
    INCONVO_API_KEY: z.string().min(1),
    NODE_ENV: z.enum(["development", "production", "test"]),
    DRIZZLE: z.enum(["TRUE", "FALSE"]).default("FALSE"),
  },
  runtimeEnv: process.env,
  emptyStringAsUndefined: true,
});
