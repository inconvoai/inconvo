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
  __INCONVO_DRIZZLE_DB__?: any;
};

class MyLogger implements Logger {
  logQuery(query: string, params: unknown[]): void {
    console.log(`Query: ${query}`);
  }
}

export async function getDb() {
  // Return cached db instance if it exists
  if (globalForDb.__INCONVO_DRIZZLE_DB__) {
    return globalForDb.__INCONVO_DRIZZLE_DB__;
  }

  const drizzleSchema = await loadDrizzleSchema();

  let db: any;

  if (env.DATABASE_DIALECT === "mysql") {
    const mysqlConn =
      globalForDb.mysqlConn ?? createPool({ uri: env.INCONVO_DATABASE_URL });
    globalForDb.mysqlConn = mysqlConn;
    db = drizzleMysql(mysqlConn, {
      schema: drizzleSchema,
      logger: env.NODE_ENV === "development" ? new MyLogger() : undefined,
      mode: "default",
    });
  } else if (env.DATABASE_DIALECT === "postgresql") {
    const pgConn = globalForDb.pgConn ?? postgres(env.INCONVO_DATABASE_URL);
    globalForDb.pgConn = pgConn;
    db = drizzlePostgres(pgConn, {
      schema: drizzleSchema,
      logger: env.NODE_ENV === "development" ? new MyLogger() : undefined,
    });
  } else {
    throw new Error(
      "Unsupported database provider. URL must start with 'mysql' or 'postgres'"
    );
  }

  // Cache the db instance with a distinct key to avoid collisions
  globalForDb.__INCONVO_DRIZZLE_DB__ = db;

  return db;
}
