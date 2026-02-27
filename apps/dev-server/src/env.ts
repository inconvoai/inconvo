import { createEnv } from "@t3-oss/env-nextjs";
import { z } from "zod";
import * as dotenv from "dotenv";
import * as path from "path";

// Load .inconvo.env file
dotenv.config({ path: path.join(process.cwd(), ".inconvo.env") });

const optionalNonEmptyTrimmedString = z
  .string()
  .optional()
  .transform((value) => {
    if (value === undefined) {
      return undefined;
    }
    const trimmed = value.trim();
    return trimmed.length > 0 ? trimmed : undefined;
  });

const optionalPositiveIntegerFromString = z
  .string()
  .optional()
  .transform((value, ctx) => {
    if (value === undefined) {
      return undefined;
    }

    const trimmed = value.trim();
    if (!trimmed) {
      return undefined;
    }

    if (!/^\d+$/.test(trimmed)) {
      ctx.addIssue({
        code: z.ZodIssueCode.custom,
        message:
          "INCONVO_BIGQUERY_MAX_BYTES_BILLED must be a positive integer when provided",
      });
      return z.NEVER;
    }

    const parsed = Number.parseInt(trimmed, 10);
    if (!Number.isInteger(parsed) || parsed <= 0) {
      ctx.addIssue({
        code: z.ZodIssueCode.custom,
        message:
          "INCONVO_BIGQUERY_MAX_BYTES_BILLED must be a positive integer when provided",
      });
      return z.NEVER;
    }

    return parsed;
  });

const shouldSkipValidation =
  !!process.env.SKIP_ENV_VALIDATION || process.env.CI === "true";

const loadedEnv = createEnv({
  server: {
    DATABASE_DIALECT: z.enum([
      "postgresql",
      "redshift",
      "mysql",
      "mssql",
      "bigquery",
    ]),
    INCONVO_DATABASE_URL: z.string().optional(),
    INCONVO_DATABASE_SCHEMA: z
      .string()
      .optional()
      .transform((val) =>
        val ? val.split(",").map((s) => s.trim()).filter(Boolean) : undefined
      ),
    INCONVO_BIGQUERY_PROJECT_ID: optionalNonEmptyTrimmedString,
    INCONVO_BIGQUERY_DATASET: optionalNonEmptyTrimmedString,
    INCONVO_BIGQUERY_LOCATION: optionalNonEmptyTrimmedString,
    INCONVO_BIGQUERY_KEYFILE: optionalNonEmptyTrimmedString,
    INCONVO_BIGQUERY_CREDENTIALS_JSON: optionalNonEmptyTrimmedString,
    INCONVO_BIGQUERY_CREDENTIALS_BASE64: optionalNonEmptyTrimmedString,
    INCONVO_BIGQUERY_MAX_BYTES_BILLED: optionalPositiveIntegerFromString,
    INCONVO_SANDBOX_BASE_URL: z.string().url(),
    INCONVO_SANDBOX_API_KEY: z.string().min(1),
    OPENAI_API_KEY: z.string().min(1),
    NODE_ENV: z
      .enum(["development", "production", "test"])
      .default("development"),
  },
  experimental__runtimeEnv: process.env,
  skipValidation: shouldSkipValidation,
});

if (!shouldSkipValidation) {
  if (loadedEnv.DATABASE_DIALECT === "bigquery") {
    if (!loadedEnv.INCONVO_BIGQUERY_PROJECT_ID) {
      throw new Error(
        "INCONVO_BIGQUERY_PROJECT_ID is required when DATABASE_DIALECT=bigquery"
      );
    }
    if (!loadedEnv.INCONVO_BIGQUERY_DATASET) {
      throw new Error(
        "INCONVO_BIGQUERY_DATASET is required when DATABASE_DIALECT=bigquery"
      );
    }
    if (!loadedEnv.INCONVO_BIGQUERY_LOCATION) {
      throw new Error(
        "INCONVO_BIGQUERY_LOCATION is required when DATABASE_DIALECT=bigquery"
      );
    }
    if (
      !loadedEnv.INCONVO_BIGQUERY_KEYFILE &&
      !loadedEnv.INCONVO_BIGQUERY_CREDENTIALS_JSON &&
      !loadedEnv.INCONVO_BIGQUERY_CREDENTIALS_BASE64
    ) {
      throw new Error(
        "Provide INCONVO_BIGQUERY_KEYFILE, INCONVO_BIGQUERY_CREDENTIALS_JSON, or INCONVO_BIGQUERY_CREDENTIALS_BASE64 when DATABASE_DIALECT=bigquery"
      );
    }
  } else if (!loadedEnv.INCONVO_DATABASE_URL) {
    throw new Error(
      `INCONVO_DATABASE_URL is required when DATABASE_DIALECT=${loadedEnv.DATABASE_DIALECT}`
    );
  }
}

export const env = loadedEnv;
