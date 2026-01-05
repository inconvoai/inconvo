import type { CompiledQuery } from "kysely";
import { logger } from "../../util/logger";

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
    throw error;
  }
}
