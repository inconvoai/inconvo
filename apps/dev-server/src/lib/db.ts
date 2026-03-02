import {
  Kysely,
  MysqlDialect,
  MssqlDialect,
  PostgresDialect,
  type LogEvent,
  type LogConfig,
} from "kysely";
import { Pool } from "pg";
import { createPool as createMysqlPool } from "mysql2";
import { env } from "~/env";

const globalForDb = globalThis as unknown as {
  __INCONVO_KYSELY_DB__?: Kysely<unknown>;
};

const createLogger = (): LogConfig => (event: LogEvent) => {
  if (event.level === "query") {
    const duration = Math.round(event.queryDurationMillis);
    console.log(`[DB] Query executed in ${duration}ms: ${event.query.sql}`);
  } else if (event.level === "error") {
    console.error("[DB] Query error:", event.error, event.query?.sql);
  }
};

export async function getDb(): Promise<Kysely<unknown>> {
  if (globalForDb.__INCONVO_KYSELY_DB__) {
    return globalForDb.__INCONVO_KYSELY_DB__;
  }

  let db: Kysely<unknown>;
  const isDevelopment = env.NODE_ENV === "development";

  if (env.DATABASE_DIALECT === "mysql") {
    const mysqlPool = createMysqlPool({
      uri: env.INCONVO_DATABASE_URL,
    });

    db = new Kysely({
      dialect: new MysqlDialect({
        pool: mysqlPool as never,
      }),
      log: isDevelopment ? createLogger() : undefined,
    });
  } else if (env.DATABASE_DIALECT === "postgresql") {
    const connectionString = env.INCONVO_DATABASE_URL;
    const poolConfig: ConstructorParameters<typeof Pool>[0] = {
      connectionString,
    };

    db = new Kysely({
      dialect: new PostgresDialect({
        pool: new Pool(poolConfig),
      }),
      log: isDevelopment ? createLogger() : undefined,
    });
  } else if (env.DATABASE_DIALECT === "mssql") {
    // Parse MSSQL connection string
    const url = new URL(env.INCONVO_DATABASE_URL);
    const [username, password] =
      url.username && url.password
        ? [url.username, url.password]
        : url.pathname.slice(1).split(":");

    /* eslint-disable @typescript-eslint/no-require-imports, @typescript-eslint/no-unsafe-assignment, @typescript-eslint/no-unsafe-member-access, @typescript-eslint/no-unsafe-call, @typescript-eslint/no-unsafe-return */
    const tedious = require("tedious");
    const tarn = require("tarn");

    db = new Kysely({
      dialect: new MssqlDialect({
        tarn: {
          ...tarn,
          options: {
            min: 0,
            max: 10,
            acquireTimeoutMillis: 60000,
            createTimeoutMillis: 30000,
            destroyTimeoutMillis: 5000,
            idleTimeoutMillis: 60000,
            propagateCreateError: true,
          },
        },
        tedious: {
          ...tedious,
          connectionFactory: async () => {
            const connection = new tedious.Connection({
              server: url.hostname,
              authentication: {
                type: "default",
                options: {
                  userName: decodeURIComponent(username ?? ""),
                  password: decodeURIComponent(password ?? ""),
                },
              },
              options: {
                port: parseInt(url.port ?? "1433"),
                database:
                  url.pathname.split("/")[1] ??
                  url.searchParams.get("database") ??
                  "master",
                encrypt: url.searchParams.get("encrypt") === "true",
                trustServerCertificate:
                  url.searchParams.get("trustServerCertificate") !== "false",
                rowCollectionOnDone: true,
                useColumnNames: false,
                connectTimeout: 30000,
              },
            });

            return connection;
          },
        },
      }),
      /* eslint-enable @typescript-eslint/no-require-imports, @typescript-eslint/no-unsafe-assignment, @typescript-eslint/no-unsafe-member-access, @typescript-eslint/no-unsafe-call, @typescript-eslint/no-unsafe-return */
      log: isDevelopment ? createLogger() : undefined,
    });
  } else {
    throw new Error(
      "Unsupported database dialect. Must be postgresql, mysql, or mssql",
    );
  }

  globalForDb.__INCONVO_KYSELY_DB__ = db;
  return db;
}
