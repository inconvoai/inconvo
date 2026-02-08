import type { SelectQueryBuilder } from "kysely";
import type { DatabaseDialect } from "../types";

export function applyLimit<DB, TB extends keyof DB, O>(
  query: SelectQueryBuilder<DB, TB, O>,
  limit: number | null | undefined,
  dialect: DatabaseDialect,
): SelectQueryBuilder<DB, TB, O> {
  if (!limit) {
    return query;
  }

  if (dialect === "mssql") {
    // MSSQL uses TOP instead of LIMIT
    return (query as any).top(limit);
  } else {
    return query.limit(limit);
  }
}
