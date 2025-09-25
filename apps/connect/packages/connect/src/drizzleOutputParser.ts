import pino from "pino";

// Strips ANSI color codes emitted by drizzle-kit so downstream logging stays plain
const stripAnsiCodes = (value: string): string =>
  value.replace(/\x1B\[[0-?]*[ -\/]*[@-~]/g, "");

export const logger = pino({
  level: process.env.LOG_LEVEL || "debug",
  transport: {
    target: "pino-pretty",
    options: {
      colorize: false,
      translateTime: "HH:MM:ss",
      ignore: "pid,hostname,time,level",
      messageFormat: "[{level}]:{msg}",
    },
  },
  formatters: {
    level: (label: string) => {
      return { level: label.toUpperCase() };
    },
  },
});

/**
 * Parses drizzle-kit pull output to extract fetched database objects
 */
function parseDrizzlePullOutput(output: string): string[] {
  const lines = output.toString().split("\n");
  const finalStateMap = new Map<string, string>();

  // Process lines to find the last occurrence of each type
  lines.forEach((line) => {
    const cleanLine = stripAnsiCodes(line);
    if (cleanLine.includes("fetched")) {
      // Extract the type (tables, columns, enums, etc.)
      const match = cleanLine.match(/(\d+\s+\w+)\s+fetched/);
      if (match) {
        finalStateMap.set(
          match[1].trim().split(/\s+/).pop() as string,
          cleanLine
        );
      }
    }
  });

  const result: string[] = [];

  // Add fetch results in a consistent order
  const order = [
    "tables",
    "columns",
    "enums",
    "indexes",
    "keys",
    "policies",
    "constraints",
    "views",
  ];

  order.forEach((key) => {
    if (finalStateMap.has(key)) {
      result.push(finalStateMap.get(key) as string);
    }
  });

  return result;
}

/**
 * Categorizes database errors into user-friendly messages
 */
function categorizeError(errorDetail: string): string {
  // Category 1: Connection errors - wrong host/port or database not accessible
  if (
    errorDetail.includes("ENOTFOUND") ||
    errorDetail.includes("ECONNREFUSED") ||
    errorDetail.includes("ETIMEDOUT") ||
    errorDetail.includes("CONNECT_TIMEOUT")
  ) {
    return `Cannot connect to database. Please verify your host, port, dialect and that the database is running and accessible.`;
  }

  // Category 2: Authentication errors - wrong username/password
  if (
    errorDetail.includes("password authentication failed") ||
    errorDetail.includes("Access denied")
  ) {
    return `Authentication failed. Please check your database username and password.`;
  }

  // Category 3: Database doesn't exist - wrong database name
  if (
    (errorDetail.includes("database") &&
      errorDetail.includes("does not exist")) ||
    errorDetail.includes("Unknown database") ||
    errorDetail.includes("ER_BAD_DB_ERROR")
  ) {
    // Try to extract database name from different error formats
    let dbName = "specified database";

    // PostgreSQL format: database "name" does not exist
    const pgMatch = errorDetail.match(/database\s+"([^"]+)"\s+does not exist/);
    if (pgMatch) {
      dbName = pgMatch[1];
    }

    // MySQL format: Unknown database 'name'
    const mysqlMatch = errorDetail.match(/Unknown database '([^']+)'/);
    if (mysqlMatch) {
      dbName = mysqlMatch[1];
    }

    return `Database "${dbName}" does not exist. Please check your database name.`;
  }

  // Return original error if no category matches
  return errorDetail;
}

/**
 * Extracts error messages from drizzle-kit output
 */
function extractErrors(output: string): {
  hasError: boolean;
  errorDetail: string;
} {
  const lines = output.split("\n");
  let hasError = false;
  const errorMessages: string[] = [];
  let captureErrorContext = false;
  let errorContextLines = 0;

  lines.forEach((line) => {
    const cleanLine = stripAnsiCodes(line);
    const trimmedLine = cleanLine.trim();

    // Start capturing when we see an Error
    if (trimmedLine.includes("Error:") || trimmedLine.includes("Error")) {
      hasError = true;
      captureErrorContext = true;
      errorContextLines = 10; // Capture next 10 lines after error
      if (trimmedLine && !errorMessages.includes(trimmedLine)) {
        errorMessages.push(trimmedLine);
      }
    }
    // Continue capturing error context lines
    else if (captureErrorContext && errorContextLines > 0 && trimmedLine) {
      errorMessages.push(trimmedLine);
      errorContextLines--;
      if (errorContextLines === 0) {
        captureErrorContext = false;
      }
    }
  });

  const errorDetail = errorMessages.join(" ");
  return { hasError, errorDetail };
}

/**
 * Processes and logs drizzle-kit pull output
 */
export function processPullOutput(output: string): void {
  // Always log the full output at debug level
  logger.debug("Full output from drizzle-kit pull:");
  output.split("\n").forEach((line) => {
    const cleanLine = stripAnsiCodes(line);
    if (cleanLine.trim()) {
      logger.debug(`  ${cleanLine.trim()}`);
    }
  });

  // Check for errors
  const { hasError, errorDetail } = extractErrors(output);

  if (hasError) {
    const categorizedError = categorizeError(errorDetail);
    throw new Error(`Error while pulling schema: ${categorizedError}`);
  }

  // Parse and display cleaned output for successful pull
  const parsedLines = parseDrizzlePullOutput(output);
  if (parsedLines.length > 0) {
    logger.info("Database introspection completed:");
    parsedLines.forEach((line) => {
      const match = line.match(/\[✓\]\s+(\d+\s+\w+\s+fetched)/);
      if (match) {
        logger.info(`[✓] ${match[1]}`);
      } else {
        logger.info(line);
      }
    });
  } else {
    // Only warn if there wasn't already an error
    logger.warn("No database objects were fetched.");
  }
}

/**
 * Logs error details from a command execution error
 */
export function logCommandError(error: any, command: string): void {
  // If the error message already contains our custom error message, just log it
  if (error.message && error.message.includes("Error while pulling schema:")) {
    logger.error(error.message);
    return;
  }

  // Otherwise, handle other types of errors
  logger.error(`Failed to run drizzle-kit ${command}`);

  // Log stderr if available
  if (error.stderr && error.stderr.toString().trim()) {
    logger.error("Error details:");
    error.stderr
      .toString()
      .split("\n")
      .forEach((line: string) => {
        if (line.trim()) {
          logger.error(`  ${line.trim()}`);
        }
      });
  }

  // Log the full error for debugging
  logger.debug(
    {
      command: `npx drizzle-kit ${command}`,
      exitCode: error.status || error.code,
      signal: error.signal,
      error: error.message,
    },
    "Full error details"
  );
}

/**
 * Logs compilation output or errors
 */
export function logCompilationResult(
  output: string | null,
  error: any
): boolean {
  if (error) {
    // Log stderr if available
    if (error.stderr) {
      error.stderr
        .toString()
        .split("\n")
        .forEach((line: string) => {
          if (line.trim()) {
            logger.error(`${line.trim()}`);
          }
        });
    }
    logger.error({ err: error }, "Failed to compile schemas");
    return false;
  }

  if (output) {
    // Split output by lines and log each non-empty line
    output
      .toString()
      .split("\n")
      .forEach((line) => {
        if (line.trim()) {
          logger.info(`${line.trim()}`);
        }
      });
  }

  logger.info("Schema pulled successfully.");
  return true;
}
