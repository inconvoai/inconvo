import { createEnv } from "@t3-oss/env-nextjs";
import { z } from "zod";
import * as dotenv from "dotenv";
import * as path from "path";

// Load .inconvo.env file
dotenv.config({ path: path.join(process.cwd(), ".inconvo.env") });

export const env = createEnv({
  server: {
    DATABASE_DIALECT: z.enum(["postgresql", "mysql", "mssql"]),
    INCONVO_DATABASE_URL: z.string().min(1),
    INCONVO_DATABASE_SCHEMA: z.string().optional(),
    INCONVO_SANDBOX_BASE_URL: z.string().url(),
    INCONVO_SANDBOX_API_KEY: z.string().min(1),
    OPENAI_API_KEY: z.string().min(1),
    LANGCHAIN_API_KEY: z.string().min(1),
    NODE_ENV: z
      .enum(["development", "production", "test"])
      .default("development"),
  },
  experimental__runtimeEnv: process.env,
  skipValidation:
    !!process.env.SKIP_ENV_VALIDATION || process.env.CI === "true",
});
