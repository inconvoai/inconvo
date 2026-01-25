// Dialects entry point - does NOT import env validation
// Use this for Lambda/serverless environments that get config from other sources
export { BigQueryDialect } from "./bigquery";
export type { BigQueryDialectConfig } from "./bigquery";
