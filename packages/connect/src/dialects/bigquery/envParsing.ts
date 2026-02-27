export function normalizeOptionalString(
  value: string | undefined | null,
): string | undefined {
  if (typeof value !== "string") {
    return undefined;
  }
  const trimmed = value.trim();
  return trimmed.length > 0 ? trimmed : undefined;
}

function parseJsonObject(
  value: string,
  variableName: "INCONVO_BIGQUERY_CREDENTIALS_JSON" | "INCONVO_BIGQUERY_CREDENTIALS_BASE64",
): Record<string, unknown> {
  let parsed: unknown;
  try {
    parsed = JSON.parse(value);
  } catch {
    throw new Error(
      `Invalid ${variableName} value. Expected a valid JSON object.`,
    );
  }

  if (!parsed || typeof parsed !== "object" || Array.isArray(parsed)) {
    throw new Error(
      `Invalid ${variableName} value. Expected a valid JSON object.`,
    );
  }

  return parsed as Record<string, unknown>;
}

export function parseBigQueryCredentialsFromEnv(input: {
  credentialsJson?: string | null;
  credentialsBase64?: string | null;
}): Record<string, unknown> | undefined {
  const credentialsJson = normalizeOptionalString(input.credentialsJson);
  if (credentialsJson) {
    return parseJsonObject(
      credentialsJson,
      "INCONVO_BIGQUERY_CREDENTIALS_JSON",
    );
  }

  const credentialsBase64 = normalizeOptionalString(input.credentialsBase64);
  if (!credentialsBase64) {
    return undefined;
  }

  const decoded = Buffer.from(credentialsBase64, "base64").toString("utf-8");
  return parseJsonObject(decoded, "INCONVO_BIGQUERY_CREDENTIALS_BASE64");
}

export function parseOptionalPositiveInteger(
  value: string | number | undefined | null,
  variableName: string,
): number | undefined {
  if (value === undefined || value === null) {
    return undefined;
  }

  if (typeof value === "number") {
    if (Number.isInteger(value) && value > 0) {
      return value;
    }
    throw new Error(`${variableName} must be a positive integer when provided`);
  }

  const trimmed = value.trim();
  if (!trimmed) {
    return undefined;
  }

  if (!/^\d+$/.test(trimmed)) {
    throw new Error(`${variableName} must be a positive integer when provided`);
  }

  const parsed = Number.parseInt(trimmed, 10);
  if (!Number.isInteger(parsed) || parsed <= 0) {
    throw new Error(`${variableName} must be a positive integer when provided`);
  }

  return parsed;
}
