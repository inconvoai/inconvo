import { sql } from "kysely";
import { getDb } from "../dbConnection";
import { env } from "../env";
import { logger } from "./logger";

const HEALTHCHECK_TIMEOUT_MS = 5_000;

// Minimal query to make sure the configured database is reachable before we start serving traffic.
// BigQuery is bypassed because it doesn't support cheap healthcheck queries like `SELECT 1`.
export async function checkDatabaseHealth(
  timeoutMs: number = HEALTHCHECK_TIMEOUT_MS,
): Promise<void> {
  // BigQuery doesn't support cheap healthcheck queries - bypass entirely
  if (env.DATABASE_DIALECT === "bigquery") {
    logger.info("Bypassing database health check for BigQuery");
    return;
  }

  const dbPromise = getDb();

  const healthCheckPromise = (async () => {
    const db = await dbPromise;
    await sql`select 1`.execute(db);
  })();

  if (!Number.isFinite(timeoutMs) || timeoutMs <= 0) {
    return healthCheckPromise;
  }

  await Promise.race([
    healthCheckPromise,
    new Promise<never>((_, reject) => {
      const id = setTimeout(() => {
        reject(
          Object.assign(
            new Error(
              `Database health check timed out after ${timeoutMs}ms (dialect: ${process.env.DATABASE_DIALECT})`,
            ),
            { code: "DB_HEALTHCHECK_TIMEOUT" },
          ),
        );
      }, timeoutMs);

      healthCheckPromise.finally(() => clearTimeout(id));
    }),
  ]);
}
