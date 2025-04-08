import { drizzle as drizzlePostgres } from "drizzle-orm/postgres-js";
import { drizzle as drizzleMysql } from "drizzle-orm/mysql2";
import { Logger } from "drizzle-orm";
import postgres from "postgres";
import { createPool, type Pool } from "mysql2/promise";
import { loadDrizzleSchema } from "~/util/loadDrizzleSchema";
import { env } from "~/env";

const globalForDb = globalThis as unknown as {
  pgConn?: postgres.Sql;
  mysqlConn?: Pool;
  db?: any;
};

class MyLogger implements Logger {
  logQuery(query: string, params: unknown[]): void {
    console.log(`Query: ${query}`);
  }
}

export async function getDb() {
  const drizzleSchema = await loadDrizzleSchema();

  let db: any;

  if (env.DATABASE_DIALECT === "mysql") {
    const mysqlConn =
      globalForDb.mysqlConn ?? createPool({ uri: env.INCONVO_DATABASE_URL });
    if (env.NODE_ENV !== "production") globalForDb.mysqlConn = mysqlConn;
    db = drizzleMysql(mysqlConn, {
      schema: drizzleSchema,
      logger: env.NODE_ENV === "development" ? new MyLogger() : undefined,
      mode: "default",
    });
  } else if (env.DATABASE_DIALECT === "postgresql") {
    const pgConn =
      globalForDb.pgConn ??
      postgres(env.INCONVO_DATABASE_URL, {
        ssl: { rejectUnauthorized: false },
      });
    if (env.NODE_ENV !== "production") globalForDb.pgConn = pgConn;
    db = drizzlePostgres(pgConn, {
      schema: drizzleSchema,
      logger: env.NODE_ENV === "development" ? new MyLogger() : undefined,
    });
  } else {
    throw new Error(
      "Unsupported database provider. URL must start with 'mysql' or 'postgres'"
    );
  }

  // Cache the db instance
  globalForDb.db = db;

  return db;
}
