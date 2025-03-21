import { drizzle as drizzlePostgres } from "drizzle-orm/postgres-js";
import { drizzle as drizzleMysql } from "drizzle-orm/mysql2";
import { Logger } from "drizzle-orm";
import postgres from "postgres";
import { createPool, type Pool } from "mysql2/promise";
import { loadDrizzleSchema } from "~/util/loadDrizzleSchema";
import assert from "assert";
import "dotenv/config";
/**
 * Cache the database connection in development. This avoids creating a new connection on every HMR
 * update.
 */
const globalForDb = globalThis as unknown as {
  pgConn?: postgres.Sql;
  mysqlConn?: Pool;
};

class MyLogger implements Logger {
  logQuery(query: string, params: unknown[]): void {
    console.log(`Query: ${query}`);
  }
}
assert(process.env.INCONVO_DATABASE_URL, "INCONVO_DATABASE_URL is not set");
const isMysql = process.env.INCONVO_DATABASE_URL.startsWith("mysql");
const isPostgres = process.env.INCONVO_DATABASE_URL.startsWith("postgres");
const drizzleSchema = loadDrizzleSchema();

let db: any;

if (isMysql) {
  const mysqlConn =
    globalForDb.mysqlConn ??
    createPool({ uri: process.env.INCONVO_DATABASE_URL });
  if (process.env.NODE_ENV !== "production") globalForDb.mysqlConn = mysqlConn;
  db = drizzleMysql(mysqlConn, {
    schema: drizzleSchema,
    logger: new MyLogger(),
    mode: "default",
  });
} else if (isPostgres) {
  const pgConn =
    globalForDb.pgConn ?? postgres(process.env.INCONVO_DATABASE_URL);
  if (process.env.NODE_ENV !== "production") globalForDb.pgConn = pgConn;

  db = drizzlePostgres(pgConn, {
    schema: drizzleSchema,
    logger: new MyLogger(),
  });
} else {
  throw new Error(
    "Unsupported database provider. URL must start with 'mysql' or 'postgres'"
  );
}

export { db };
