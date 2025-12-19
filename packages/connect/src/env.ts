import "dotenv/config";
import { createEnv } from "@t3-oss/env-core";
import { z } from "zod";

const SQL_DIALECTS = ["postgresql", "redshift", "mysql", "mssql"] as const;
const ALL_DIALECTS = [...SQL_DIALECTS, "bigquery"] as const;

export const env = createEnv({
  server: {
    DATABASE_DIALECT: z.enum(ALL_DIALECTS),
    INCONVO_DATABASE_URL: z.string().optional(),
    INCONVO_DATABASE_SCHEMA: z.string().optional(),
    INCONVO_SECRET_KEY: z.string(),
    NODE_ENV: z.enum(["development", "production", "test"]),
    INCONVO_BIGQUERY_PROJECT_ID: z.string().optional(),
    INCONVO_BIGQUERY_DATASET: z.string().optional(),
    INCONVO_BIGQUERY_LOCATION: z.string().optional(),
    INCONVO_BIGQUERY_CREDENTIALS_JSON: z.string().optional(),
    INCONVO_BIGQUERY_KEYFILE: z.string().optional(),
    INCONVO_BIGQUERY_MAX_BYTES_BILLED: z.coerce.number().optional(),
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
        INCONVO_BIGQUERY_PROJECT_ID: z.string(),
        INCONVO_BIGQUERY_DATASET: z.string(),
        INCONVO_BIGQUERY_LOCATION: z.string(),
        INCONVO_BIGQUERY_MAX_BYTES_BILLED: z.coerce.number().optional(),
        INCONVO_BIGQUERY_CREDENTIALS_JSON: z.string().optional(),
        INCONVO_BIGQUERY_KEYFILE: z.string().optional(),
      })
      .refine(
        (value) =>
          Boolean(
            value.INCONVO_BIGQUERY_CREDENTIALS_JSON ||
            value.INCONVO_BIGQUERY_KEYFILE,
          ),
        {
          path: ["INCONVO_BIGQUERY_CREDENTIALS_JSON"],
          message:
            "Provide INCONVO_BIGQUERY_CREDENTIALS_JSON or INCONVO_BIGQUERY_KEYFILE",
        },
      );

    return z.discriminatedUnion("DATABASE_DIALECT", [
      bigQuerySchema,
      sqlSchema,
    ]);
  },
});
