import "dotenv/config";
import { createEnv } from "@t3-oss/env-core";
import { z } from "zod";

export const env = createEnv({
  server: {
    DATABASE_DIALECT: z.enum(["postgresql", "mysql"]),
    INCONVO_DATABASE_URL: z.string(),
    INCONVO_SECRET_KEY: z.string(),
    NODE_ENV: z.enum(["development", "production", "test"]),
  },
  runtimeEnv: process.env,
  emptyStringAsUndefined: true,
});
