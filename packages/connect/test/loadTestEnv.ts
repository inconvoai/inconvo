import fs from "fs";
import path from "path";
import { fileURLToPath } from "url";
import { config as loadDotenv } from "dotenv";
import type { OperationContext, DatabaseDialect } from "../src/operations/types";

const __dirname = path.dirname(fileURLToPath(import.meta.url));

type SupportedDialect = "mysql" | "postgresql" | "mssql" | "bigquery";

const DIALECT_DIR: Record<SupportedDialect, string> = {
  mysql: "mysql",
  postgresql: "pg",
  mssql: "mssql",
  bigquery: "bigquery",
};

const SQL_DIALECTS = new Set<SupportedDialect>([
  "mysql",
  "postgresql",
  "mssql",
]);

export function loadTestEnv(dialect: SupportedDialect) {
  const candidates = [
    path.resolve(__dirname, `${dialect}.env`),
    path.resolve(__dirname, DIALECT_DIR[dialect], `${dialect}.env`),
  ];

  for (const candidate of candidates) {
    if (fs.existsSync(candidate)) {
      loadDotenv({ path: candidate, override: true });
      break;
    }
  }

  process.env.DATABASE_DIALECT = dialect === "bigquery" ? "bigquery" : dialect;
  process.env.NODE_ENV = process.env.NODE_ENV ?? "test";

  if (SQL_DIALECTS.has(dialect) && !process.env.INCONVO_DATABASE_URL) {
    throw new Error(
      `INCONVO_DATABASE_URL is required for ${dialect} tests. Create a ${dialect}.env file under packages/connect/test or set the variable in your shell.`,
    );
  }

  if (dialect === "bigquery") {
    const missing: string[] = [];
    const required = [
      "INCONVO_BIGQUERY_PROJECT_ID",
      "INCONVO_BIGQUERY_DATASET",
      "INCONVO_BIGQUERY_LOCATION",
    ] as const;

    for (const key of required) {
      if (!process.env[key]) {
        missing.push(key);
      }
    }

    if (
      !process.env.INCONVO_BIGQUERY_CREDENTIALS_JSON &&
      !process.env.INCONVO_BIGQUERY_KEYFILE
    ) {
      missing.push(
        "INCONVO_BIGQUERY_KEYFILE (or INCONVO_BIGQUERY_CREDENTIALS_JSON)",
      );
    }

    if (missing.length > 0) {
      throw new Error(
        `Missing BigQuery test configuration: ${missing.join(
          ", ",
        )}. Create a bigquery.env file under packages/connect/test or set the variables in your shell.`,
      );
    }
  }
}

/**
 * Get the OperationContext for tests.
 * Must be called after loadTestEnv() and after importing getAugmentedSchema.
 */
export async function getTestContext(): Promise<OperationContext> {
  const { getAugmentedSchema } = await import("../src/util/augmentedSchemaCache");
  const schema = await getAugmentedSchema();
  const dialect = process.env.DATABASE_DIALECT as DatabaseDialect;
  return { schema, dialect };
}
