import { drizzle } from "drizzle-orm/postgres-js";
import postgres from "postgres";
import { env } from "~/env";
import * as schema from "~/../drizzle/schema";
import * as drizzleRelations from "~/../drizzle/relations";
import { Logger } from "drizzle-orm";

/**
 * Cache the database connection in development. This avoids creating a new connection on every HMR
 * update.
 */
const globalForDb = globalThis as unknown as {
  conn: postgres.Sql | undefined;
};

const conn = globalForDb.conn ?? postgres(env.INCONVO_DATABASE_URL);
if (env.NODE_ENV !== "production") globalForDb.conn = conn;

class MyLogger implements Logger {
  logQuery(query: string, params: unknown[]): void {
    console.log({ query });
  }
}

export const db = drizzle(conn, {
  schema: { ...schema, ...drizzleRelations },
  logger: new MyLogger(),
});
