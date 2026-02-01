import type { CompiledQuery } from "kysely";
import { logger } from "../../util/logger";
import { QueryExecutionError } from "../../util/queryErrors";
import type { QueryExecutionErrorDetails } from "@repo/types";

/**
 * Interface for Kysely query objects that can be both compiled and executed.
 */
interface ExecutableQuery<T> {
  compile(): CompiledQuery;
  execute(): Promise<T[]>;
}

export interface QueryExecutionResult<T> {
  rows: T[];
  compiled: CompiledQuery;
}

function extractDbErrorDetails(error: unknown): {
  code?: string;
  detail?: string;
  hint?: string;
} {
  if (!error || typeof error !== "object") {
    return {};
  }

  const record = error as Record<string, unknown>;
  return {
    code: typeof record.code === "string" ? record.code : undefined,
    detail: typeof record.detail === "string" ? record.detail : undefined,
    hint: typeof record.hint === "string" ? record.hint : undefined,
  };
}

/**
 * Executes a Kysely query and logs the SQL on error.
 * Compiles the query before execution so we have the SQL available for error logging.
 */
export async function executeWithLogging<T>(
  query: ExecutableQuery<T>,
  context?: { operation?: string },
): Promise<QueryExecutionResult<T>> {
  const compiled = query.compile();

  try {
    const rows = await query.execute();
    return { rows, compiled };
  } catch (error) {
    logger.error(
      {
        sql: compiled.sql,
        params: compiled.parameters,
        operation: context?.operation,
        error,
      },
      "Query execution failed",
    );
    const message = error instanceof Error ? error.message : String(error);
    const details: QueryExecutionErrorDetails = {
      type: "query_execution",
      message,
      sql: compiled.sql,
      params: Array.from(compiled.parameters),
      operation: context?.operation,
      ...extractDbErrorDetails(error),
    };
    throw new QueryExecutionError(details, error);
  }
}
