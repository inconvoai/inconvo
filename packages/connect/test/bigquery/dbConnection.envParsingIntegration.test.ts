import { afterEach, beforeEach, describe, expect, it, jest } from "@jest/globals";

function applyBaseBigQueryEnv(): void {
  process.env.DATABASE_DIALECT = "bigquery";
  process.env.NODE_ENV = "test";
  process.env.INCONVO_SECRET_KEY = "test-secret-key";
  process.env.INCONVO_BIGQUERY_PROJECT_ID = "project-id";
  process.env.INCONVO_BIGQUERY_DATASET = "dataset-name";
  process.env.INCONVO_BIGQUERY_LOCATION = " EU ";
  process.env.INCONVO_BIGQUERY_KEYFILE = "   ";
}

function clearBigQueryEnv(): void {
  delete process.env.DATABASE_DIALECT;
  delete process.env.NODE_ENV;
  delete process.env.INCONVO_SECRET_KEY;
  delete process.env.INCONVO_BIGQUERY_PROJECT_ID;
  delete process.env.INCONVO_BIGQUERY_DATASET;
  delete process.env.INCONVO_BIGQUERY_LOCATION;
  delete process.env.INCONVO_BIGQUERY_CREDENTIALS_JSON;
  delete process.env.INCONVO_BIGQUERY_CREDENTIALS_BASE64;
  delete process.env.INCONVO_BIGQUERY_KEYFILE;
  delete process.env.INCONVO_BIGQUERY_MAX_BYTES_BILLED;
}

describe("dbConnection BigQuery env parsing integration", () => {
  beforeEach(() => {
    clearBigQueryEnv();
    (
      globalThis as {
        __INCONVO_KYSELY_DB__?: unknown;
      }
    ).__INCONVO_KYSELY_DB__ = undefined;
    jest.resetModules();
  });

  afterEach(() => {
    clearBigQueryEnv();
    (
      globalThis as {
        __INCONVO_KYSELY_DB__?: unknown;
      }
    ).__INCONVO_KYSELY_DB__ = undefined;
  });

  it("accepts base64 credentials when JSON env is whitespace", async () => {
    applyBaseBigQueryEnv();
    process.env.INCONVO_BIGQUERY_CREDENTIALS_JSON = "   ";
    process.env.INCONVO_BIGQUERY_CREDENTIALS_BASE64 = Buffer.from(
      JSON.stringify({ client_email: "service-account@example.com" }),
      "utf-8",
    ).toString("base64");
    process.env.INCONVO_BIGQUERY_MAX_BYTES_BILLED = "1000";

    const { getDb } = await import("../../src/dbConnection");
    await expect(getDb()).resolves.toBeDefined();
  });

  it("surfaces base64 parsing errors through dbConnection", async () => {
    applyBaseBigQueryEnv();
    process.env.INCONVO_BIGQUERY_CREDENTIALS_JSON = "   ";
    process.env.INCONVO_BIGQUERY_CREDENTIALS_BASE64 = "not-json";

    const { getDb } = await import("../../src/dbConnection");
    await expect(getDb()).rejects.toThrow(
      "Invalid INCONVO_BIGQUERY_CREDENTIALS_BASE64 value. Expected a valid JSON object.",
    );
  });
});
