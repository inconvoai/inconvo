import {
  Kysely,
  MysqlDialect,
  MssqlDialect,
  PostgresDialect,
  type LogEvent,
  type LogConfig,
} from "kysely";
import {
  BigQueryDialect,
  type BigQueryDialectConfig,
} from "@repo/connect/dialects";
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

function requireDatabaseUrl(): string {
  if (!env.INCONVO_DATABASE_URL) {
    throw new Error("INCONVO_DATABASE_URL is required for SQL dialects");
  }
  return env.INCONVO_DATABASE_URL;
}

function parseBigQueryCredentials():
  | Record<string, unknown>
  | undefined {
  const credentialsJson = env.INCONVO_BIGQUERY_CREDENTIALS_JSON?.trim();
  const credentialsBase64 = env.INCONVO_BIGQUERY_CREDENTIALS_BASE64?.trim();
  const serializedCredentials =
    credentialsJson ??
    (credentialsBase64
      ? Buffer.from(credentialsBase64, "base64").toString("utf-8")
      : undefined);

  if (!serializedCredentials) {
    return undefined;
  }

  try {
    return JSON.parse(serializedCredentials) as Record<string, unknown>;
  } catch {
    throw new Error(
      "Invalid BigQuery credentials. Provide valid INCONVO_BIGQUERY_CREDENTIALS_JSON or INCONVO_BIGQUERY_CREDENTIALS_BASE64."
    );
  }
}

export async function getDb(): Promise<Kysely<unknown>> {
  if (globalForDb.__INCONVO_KYSELY_DB__) {
    return globalForDb.__INCONVO_KYSELY_DB__;
  }

  let db: Kysely<unknown>;
  const isDevelopment = env.NODE_ENV === "development";

  if (env.DATABASE_DIALECT === "mysql") {
    const mysqlPool = createMysqlPool({
      uri: requireDatabaseUrl(),
    });

    db = new Kysely({
      dialect: new MysqlDialect({
        pool: mysqlPool as never,
      }),
      log: isDevelopment ? createLogger() : undefined,
    });
  } else if (
    env.DATABASE_DIALECT === "postgresql" ||
    env.DATABASE_DIALECT === "redshift"
  ) {
    const connectionString = requireDatabaseUrl();
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
    const url = new URL(requireDatabaseUrl());
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
  } else if (env.DATABASE_DIALECT === "bigquery") {
    if (!env.INCONVO_BIGQUERY_PROJECT_ID) {
      throw new Error("INCONVO_BIGQUERY_PROJECT_ID is required for BigQuery");
    }
    if (!env.INCONVO_BIGQUERY_DATASET) {
      throw new Error("INCONVO_BIGQUERY_DATASET is required for BigQuery");
    }
    if (!env.INCONVO_BIGQUERY_LOCATION) {
      throw new Error("INCONVO_BIGQUERY_LOCATION is required for BigQuery");
    }
    if (
      !env.INCONVO_BIGQUERY_KEYFILE &&
      !env.INCONVO_BIGQUERY_CREDENTIALS_JSON &&
      !env.INCONVO_BIGQUERY_CREDENTIALS_BASE64
    ) {
      throw new Error(
        "Provide INCONVO_BIGQUERY_KEYFILE, INCONVO_BIGQUERY_CREDENTIALS_JSON, or INCONVO_BIGQUERY_CREDENTIALS_BASE64 for BigQuery"
      );
    }

    const dialectConfig: BigQueryDialectConfig = {
      projectId: env.INCONVO_BIGQUERY_PROJECT_ID,
      dataset: env.INCONVO_BIGQUERY_DATASET,
      location: env.INCONVO_BIGQUERY_LOCATION,
      keyFilename: env.INCONVO_BIGQUERY_KEYFILE,
      credentials: parseBigQueryCredentials(),
      maximumBytesBilled: env.INCONVO_BIGQUERY_MAX_BYTES_BILLED,
    };

    db = new Kysely({
      dialect: new BigQueryDialect(dialectConfig),
      log: isDevelopment ? createLogger() : undefined,
    });
  } else {
    throw new Error(
      "Unsupported database dialect for dev-server local DB helper",
    );
  }

  globalForDb.__INCONVO_KYSELY_DB__ = db;
  return db;
}
