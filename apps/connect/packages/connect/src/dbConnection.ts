import { drizzle as drizzlePostgres } from "drizzle-orm/postgres-js";
import { drizzle as drizzleMysql } from "drizzle-orm/mysql2";
import { env } from "~/env";
import { Logger } from "drizzle-orm";
import postgres from "postgres";
import { createPool, type Pool } from "mysql2/promise";

import * as schema from "~/../drizzle/schema";
import * as drizzleRelations from "~/../drizzle/relations";

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

const isMysql = env.INCONVO_DATABASE_URL.startsWith("mysql");
const isPostgres = env.INCONVO_DATABASE_URL.startsWith("postgres");

let db: any;

if (isMysql) {
  const mysqlConn =
    globalForDb.mysqlConn ?? createPool({ uri: env.INCONVO_DATABASE_URL });
  if (env.NODE_ENV !== "production") globalForDb.mysqlConn = mysqlConn;
  db = drizzleMysql(mysqlConn, {
    schema: { ...schema, ...drizzleRelations },
    logger: new MyLogger(),
    mode: "default",
  });
} else if (isPostgres) {
  const pgConn = globalForDb.pgConn ?? postgres(env.INCONVO_DATABASE_URL);
  if (env.NODE_ENV !== "production") globalForDb.pgConn = pgConn;

  db = drizzlePostgres(pgConn, {
    schema: { ...schema, ...drizzleRelations },
    logger: new MyLogger(),
  });
} else {
  throw new Error(
    "Unsupported database provider. URL must start with 'mysql' or 'postgres'"
  );
}

export { db };
