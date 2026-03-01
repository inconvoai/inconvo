import { describe, expect, it } from "@jest/globals";
import {
  parseBigQueryCredentialsFromEnv,
  parseOptionalPositiveInteger,
} from "../../src/dialects/bigquery/envParsing";

describe("parseBigQueryCredentialsFromEnv", () => {
  it("falls back to base64 when JSON value is empty", () => {
    const expected = {
      type: "service_account",
      project_id: "proj",
    };
    const credentialsBase64 = Buffer.from(JSON.stringify(expected)).toString(
      "base64",
    );

    const parsed = parseBigQueryCredentialsFromEnv({
      credentialsJson: "",
      credentialsBase64,
    });

    expect(parsed).toEqual(expected);
  });

  it("falls back to base64 when JSON value is whitespace", () => {
    const expected = {
      type: "service_account",
      project_id: "proj",
    };
    const credentialsBase64 = Buffer.from(JSON.stringify(expected)).toString(
      "base64",
    );

    const parsed = parseBigQueryCredentialsFromEnv({
      credentialsJson: "   ",
      credentialsBase64,
    });

    expect(parsed).toEqual(expected);
  });

  it("throws for invalid JSON credentials", () => {
    expect(() =>
      parseBigQueryCredentialsFromEnv({
        credentialsJson: "{invalid",
      }),
    ).toThrow(
      "Invalid INCONVO_BIGQUERY_CREDENTIALS_JSON value. Expected a valid JSON object.",
    );
  });

  it("throws for invalid base64 credentials payload", () => {
    expect(() =>
      parseBigQueryCredentialsFromEnv({
        credentialsBase64: "not-json",
      }),
    ).toThrow(
      "Invalid INCONVO_BIGQUERY_CREDENTIALS_BASE64 value. Expected a valid JSON object.",
    );
  });
});

describe("parseOptionalPositiveInteger", () => {
  it("returns undefined for empty values", () => {
    expect(parseOptionalPositiveInteger(undefined, "TEST")).toBeUndefined();
    expect(parseOptionalPositiveInteger("", "TEST")).toBeUndefined();
    expect(parseOptionalPositiveInteger("   ", "TEST")).toBeUndefined();
  });

  it("parses positive integers", () => {
    expect(parseOptionalPositiveInteger("1000", "TEST")).toBe(1000);
    expect(parseOptionalPositiveInteger(42, "TEST")).toBe(42);
  });

  it("throws for zero, negative, and invalid values", () => {
    expect(() => parseOptionalPositiveInteger("0", "TEST")).toThrow(
      "TEST must be a positive integer when provided",
    );
    expect(() => parseOptionalPositiveInteger("-1", "TEST")).toThrow(
      "TEST must be a positive integer when provided",
    );
    expect(() => parseOptionalPositiveInteger("12.5", "TEST")).toThrow(
      "TEST must be a positive integer when provided",
    );
    expect(() => parseOptionalPositiveInteger("abc", "TEST")).toThrow(
      "TEST must be a positive integer when provided",
    );
  });
});
