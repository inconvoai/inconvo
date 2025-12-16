import { logger } from "~/util/logger";

type ErrorLike = {
  code?: string | number;
  errno?: string | number;
  message?: string;
  name?: string;
  cause?: unknown;
  [key: string]: unknown;
};

interface FailurePattern {
  matches: (error: ErrorLike) => boolean;
  hint: (context: string) => string;
}

const normalizeCode = (value?: string | number): string | undefined => {
  if (value === undefined || value === null) {
    return undefined;
  }
  return String(value).toUpperCase();
};

const messageIncludes = (error: ErrorLike, pattern: RegExp): boolean => {
  const rawMessage = error.message;
  if (rawMessage === undefined || rawMessage === null) {
    return false;
  }
  const message =
    typeof rawMessage === "string" ? rawMessage : String(rawMessage);
  return pattern.test(message);
};

const codeMatches = (
  error: ErrorLike,
  ...codes: Array<string | number>
): boolean => {
  const normalizedCode = normalizeCode(error.code);
  const normalizedErrno = normalizeCode(error.errno);
  return codes.some((code) => {
    const normalized = normalizeCode(code);
    return normalizedCode === normalized || normalizedErrno === normalized;
  });
};

const failurePatterns: FailurePattern[] = [
  {
    matches: (error) =>
      codeMatches(error, "ENOTFOUND", "EAI_AGAIN") ||
      messageIncludes(error, /getaddrinfo .*? (?:enotfound|eai_again)/i),
    hint: (context) =>
      `${context} could not resolve the database host. Verify the hostname and DNS settings, or confirm any VPNs are connected.`,
  },
  {
    matches: (error) =>
      codeMatches(error, "EHOSTUNREACH", "ENETUNREACH", "EHOSTDOWN") ||
      messageIncludes(error, /no route to host/i),
    hint: (context) =>
      `${context} cannot reach the database host (no route to host). Confirm network/VPC connectivity and firewall allowlists.`,
  },
  {
    matches: (error) =>
      codeMatches(error, "ECONNREFUSED") ||
      messageIncludes(error, /connection (?:was )?refused/i),
    hint: (context) =>
      `${context} was refused by the database port. Ensure the service is running and security groups allow inbound connections.`,
  },
  {
    matches: (error) =>
      codeMatches(error, "ETIMEDOUT") ||
      messageIncludes(error, /timeout(?:ed)?/i),
    hint: (context) =>
      `${context} timed out before the database responded. Check network latency, firewall rules, or increase the timeout.`,
  },
  {
    matches: (error) =>
      codeMatches(
        error,
        "28P01", // Postgres invalid_password
        "ER_ACCESS_DENIED_ERROR",
        "ELOGIN"
      ) ||
      messageIncludes(
        error,
        /password authentication failed|access denied for user|login failed/i
      ),
    hint: (context) =>
      `${context} could not authenticate with the database. Confirm the username, password, and any rotation of credentials.`,
  },
  {
    matches: (error) =>
      messageIncludes(
        error,
        /self[- ]signed certificate|certificate verify failed|ssl handshake/i
      ),
    hint: (context) =>
      `${context} hit an SSL/TLS validation error. Double-check the certificate chain or trust settings for your database.`,
  },
  {
    matches: (error) =>
      messageIncludes(error, /no pg_hba\.conf entry/i) ||
      messageIncludes(error, /pg_hba\.conf/i),
    hint: (context) =>
      `${context} was blocked by pg_hba.conf. Add the connector's IP or user entry to the database allowlist.`,
  },
];

const schemaDefaultHint =
  "Schema preload failed before serving requests. Validate database connectivity, credentials, and firewall allowlists.";
const healthCheckDefaultHint =
  "Database health check could not reach the configured database. Confirm network/VPC access, firewall rules, and credentials.";

function unwrapError(error: unknown): ErrorLike {
  if (!error) {
    return {};
  }

  const currentError = error as ErrorLike;

  if (currentError.cause) {
    const nested = unwrapError(currentError.cause);
    return {
      ...currentError,
      code: currentError.code ?? nested.code,
      errno: currentError.errno ?? nested.errno,
      message: currentError.message ?? nested.message,
    };
  }

  return currentError;
}

function resolveFailureHint(
  error: unknown,
  context: string,
  fallback: string
): string {
  const normalizedError = unwrapError(error);

  for (const pattern of failurePatterns) {
    if (pattern.matches(normalizedError)) {
      return pattern.hint(context);
    }
  }

  return fallback;
}

export function logSchemaPreloadFailureHint(error: unknown): void {
  logger.info(resolveFailureHint(error, "Schema preload", schemaDefaultHint));
}

export function logDatabaseHealthCheckHint(error: unknown): void {
  logger.info(
    resolveFailureHint(error, "Database health check", healthCheckDefaultHint)
  );
}
