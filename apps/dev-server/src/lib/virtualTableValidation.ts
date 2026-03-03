import type { DatabaseDialect } from "@repo/connect";
import type { ValidateVirtualTableResponse } from "@repo/types";

const VALID_DIALECTS: DatabaseDialect[] = [
  "postgresql",
  "redshift",
  "mysql",
  "mssql",
  "bigquery",
];

export function isDatabaseDialect(value: string): value is DatabaseDialect {
  return VALID_DIALECTS.includes(value as DatabaseDialect);
}

export function getDatabaseDialectErrorMessage(): string {
  return `dialect must be one of: ${VALID_DIALECTS.join(", ")}`;
}

export async function validateVirtualTableSql(params: {
  sql: string;
  requestDialect?: string;
  previewLimit?: number;
}): Promise<{ dialect: DatabaseDialect; result: ValidateVirtualTableResponse }> {
  const { env, getDb, validateVirtualTableCore, QueryExecutionError } = await import(
    "@repo/connect"
  );

  const db = await getDb();
  const requestDialect =
    params.requestDialect && isDatabaseDialect(params.requestDialect)
      ? params.requestDialect
      : undefined;

  try {
    const result = await validateVirtualTableCore({
      sql: params.sql,
      dialect: env.DATABASE_DIALECT,
      requestDialect,
      previewLimit: params.previewLimit ?? 5,
      db,
    });

    return { dialect: env.DATABASE_DIALECT, result };
  } catch (error) {
    if (error instanceof QueryExecutionError) {
      return {
        dialect: env.DATABASE_DIALECT,
        result: {
          ok: false,
          error: {
            message: error.details.message,
            sql: error.details.sql,
            code: error.details.code,
            detail: error.details.detail,
            hint: error.details.hint,
          },
        },
      };
    }

    throw error;
  }
}
