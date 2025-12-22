import type { SelectQueryBuilder } from "kysely";
import { env } from "../../env";

export function applyLimit<DB, TB extends keyof DB, O>(
  query: SelectQueryBuilder<DB, TB, O>,
  limit: number | null | undefined,
): SelectQueryBuilder<DB, TB, O> {
  if (!limit) {
    return query;
  }

  if (env.DATABASE_DIALECT === "mssql") {
    // MSSQL uses TOP instead of LIMIT
    return (query as any).top(limit);
  } else {
    return query.limit(limit);
  }
}
