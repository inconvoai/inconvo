// Jest setup file
jest.setTimeout(30000);

// Set test-scoped schema augmentations directory
// This must be set before any modules are imported to ensure
// schemaAugmentationStore.ts reads from the test fixtures
process.env.SCHEMA_AUGMENTATIONS_DIR = "./test/fixtures/schema-augmentations";

// Mock the env module to avoid ES module issues
jest.mock("~/env", () => ({
  get env() {
    return {
      SCHEMA_AUGMENTATIONS_DIR: "./test/fixtures/schema-augmentations",
      DATABASE_DIALECT: process.env.DATABASE_DIALECT || "mssql",
      INCONVO_DATABASE_URL:
        process.env.INCONVO_DATABASE_URL ||
        "mssql://sa:Notroot123@localhost:1433/hosted",
      INCONVO_DATABASE_SCHEMA: process.env.INCONVO_DATABASE_SCHEMA,
      INCONVO_SECRET_KEY: process.env.INCONVO_SECRET_KEY || "test-secret-key",
      NODE_ENV: process.env.NODE_ENV || "test",
      INCONVO_BIGQUERY_PROJECT_ID: process.env.INCONVO_BIGQUERY_PROJECT_ID,
      INCONVO_BIGQUERY_DATASET: process.env.INCONVO_BIGQUERY_DATASET,
      INCONVO_BIGQUERY_LOCATION: process.env.INCONVO_BIGQUERY_LOCATION,
      INCONVO_BIGQUERY_CREDENTIALS_JSON:
        process.env.INCONVO_BIGQUERY_CREDENTIALS_JSON,
      INCONVO_BIGQUERY_KEYFILE: process.env.INCONVO_BIGQUERY_KEYFILE,
      INCONVO_BIGQUERY_MAX_BYTES_BILLED: process.env
        .INCONVO_BIGQUERY_MAX_BYTES_BILLED
        ? Number(process.env.INCONVO_BIGQUERY_MAX_BYTES_BILLED)
        : undefined,
    };
  },
}));
