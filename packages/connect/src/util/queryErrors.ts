import type { QueryExecutionErrorDetails } from "@repo/types";

export class QueryExecutionError extends Error {
  details: QueryExecutionErrorDetails;

  constructor(details: QueryExecutionErrorDetails, cause?: unknown) {
    super(details.message, { cause });
    this.name = "QueryExecutionError";
    this.details = details;
  }
}
