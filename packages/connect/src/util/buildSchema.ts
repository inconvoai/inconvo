import type { SchemaResponse } from "../types/types";
import { getDb } from "../dbConnection";
import { env } from "../env";
import { logger } from "./logger";
import {
  buildSchemaFromDb,
  type IntrospectionConfig,
  type IntrospectionLogger,
} from "./buildSchemaFromDb";

// Re-export for convenience
export { buildSchemaFromDb } from "./buildSchemaFromDb";
export type {
  IntrospectionConfig,
  IntrospectionDialect,
  IntrospectionLogger,
} from "./buildSchemaFromDb";

/**
 * Adapter to convert pino logger to IntrospectionLogger interface.
 */
const introspectionLogger: IntrospectionLogger = {
  info: (data, message) => logger.info(data, message),
  warn: (data, message) => logger.warn(data, message),
};

/**
 * Build database schema by introspecting the database.
 * Uses environment variables for configuration.
 *
 * For Lambda/serverless use cases where config comes from other sources,
 * use buildSchemaFromDb() directly.
 */
export async function buildSchema(): Promise<SchemaResponse> {
  const db = await getDb();

  const config: IntrospectionConfig = {
    dialect: env.DATABASE_DIALECT,
    databaseSchema: env.INCONVO_DATABASE_SCHEMA,
  };

  // Add BigQuery-specific config if needed
  if (env.DATABASE_DIALECT === "bigquery") {
    let credentials: Record<string, unknown> | undefined;
    if (env.INCONVO_BIGQUERY_CREDENTIALS_JSON) {
      try {
        credentials = JSON.parse(env.INCONVO_BIGQUERY_CREDENTIALS_JSON);
      } catch {
        // Will be logged by buildSchemaFromDb
      }
    }

    config.bigQuery = {
      projectId: env.INCONVO_BIGQUERY_PROJECT_ID!,
      dataset: env.INCONVO_BIGQUERY_DATASET!,
      location: env.INCONVO_BIGQUERY_LOCATION,
      credentials,
      keyFilename: env.INCONVO_BIGQUERY_KEYFILE,
    };
  }

  return buildSchemaFromDb(db, config, introspectionLogger);
}
