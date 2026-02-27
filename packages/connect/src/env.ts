import "dotenv/config";
import { createEnv } from "@t3-oss/env-core";
import { z } from "zod";
import { parseOptionalPositiveInteger } from "./dialects/bigquery/envParsing";

const SQL_DIALECTS = ["postgresql", "redshift", "mysql", "mssql"] as const;
const ALL_DIALECTS = [...SQL_DIALECTS, "bigquery"] as const;

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
    try {
      return parseOptionalPositiveInteger(
        value,
        "INCONVO_BIGQUERY_MAX_BYTES_BILLED",
      );
    } catch (error) {
      ctx.addIssue({
        code: z.ZodIssueCode.custom,
        message:
          error instanceof Error
            ? error.message
            : "INCONVO_BIGQUERY_MAX_BYTES_BILLED must be a positive integer when provided",
      });
      return z.NEVER;
    }
  });

// Skip validation during build (CI or when explicitly set)
const shouldSkipValidation =
  !!process.env.SKIP_ENV_VALIDATION || process.env.CI === "true";

export const env = createEnv({
  skipValidation: shouldSkipValidation,
  server: {
    DATABASE_DIALECT: z.enum(ALL_DIALECTS),
    INCONVO_DATABASE_URL: z.string().optional(),
    INCONVO_DATABASE_SCHEMA: z
      .string()
      .optional()
      .transform((val) =>
        val ? val.split(",").map((s) => s.trim()).filter(Boolean) : undefined
      ),
    INCONVO_SECRET_KEY: z.string(),
    NODE_ENV: z.enum(["development", "production", "test"]),
    INCONVO_BIGQUERY_PROJECT_ID: optionalNonEmptyTrimmedString,
    INCONVO_BIGQUERY_DATASET: optionalNonEmptyTrimmedString,
    INCONVO_BIGQUERY_LOCATION: optionalNonEmptyTrimmedString,
    INCONVO_BIGQUERY_CREDENTIALS_JSON: optionalNonEmptyTrimmedString,
    INCONVO_BIGQUERY_CREDENTIALS_BASE64: optionalNonEmptyTrimmedString,
    INCONVO_BIGQUERY_KEYFILE: optionalNonEmptyTrimmedString,
    INCONVO_BIGQUERY_MAX_BYTES_BILLED: optionalPositiveIntegerFromString,
  },
  runtimeEnv: process.env,
  emptyStringAsUndefined: true,
  createFinalSchema: (shape) => {
    const base = z.object(shape);
    const sqlSchema = base.extend({
      DATABASE_DIALECT: z.enum(SQL_DIALECTS),
      INCONVO_DATABASE_URL: z.string(),
    });
    const bigQuerySchema = base
      .extend({
        DATABASE_DIALECT: z.literal("bigquery"),
        INCONVO_BIGQUERY_PROJECT_ID: z.string().trim().min(1),
        INCONVO_BIGQUERY_DATASET: z.string().trim().min(1),
        INCONVO_BIGQUERY_LOCATION: z.string().trim().min(1),
        INCONVO_BIGQUERY_MAX_BYTES_BILLED: optionalPositiveIntegerFromString,
        INCONVO_BIGQUERY_CREDENTIALS_JSON: optionalNonEmptyTrimmedString,
        INCONVO_BIGQUERY_CREDENTIALS_BASE64: optionalNonEmptyTrimmedString,
        INCONVO_BIGQUERY_KEYFILE: optionalNonEmptyTrimmedString,
      })
      .refine(
        (value) =>
          Boolean(
            value.INCONVO_BIGQUERY_CREDENTIALS_JSON ||
              value.INCONVO_BIGQUERY_CREDENTIALS_BASE64 ||
              value.INCONVO_BIGQUERY_KEYFILE,
          ),
        {
          path: ["INCONVO_BIGQUERY_CREDENTIALS_JSON"],
          message:
            "Provide INCONVO_BIGQUERY_CREDENTIALS_JSON, INCONVO_BIGQUERY_CREDENTIALS_BASE64, or INCONVO_BIGQUERY_KEYFILE",
        },
      );

    return z.discriminatedUnion("DATABASE_DIALECT", [
      bigQuerySchema,
      sqlSchema,
    ]);
  },
});
