import {
  Kysely,
  MysqlDialect,
  MssqlDialect,
  PostgresDialect,
} from "kysely";
import type { LogEvent, LogConfig, MysqlDialectConfig } from "kysely";
import { Pool } from "pg";
import { createPool as createMysqlPool } from "mysql2";
import { env } from "./env";
import { logger } from "./util/logger";
import {
  BigQueryDialect,
  type BigQueryDialectConfig,
} from "./dialects/bigquery";

const globalForDb = globalThis as unknown as {
  __INCONVO_KYSELY_DB__?: Kysely<any>;
};

const createLogger = (): LogConfig => (event: LogEvent) => {
  if (event.level === "query") {
    const duration = Math.round(event.queryDurationMillis);
    logger.debug(
      { sql: event.query.sql, duration },
      `Query executed in ${duration}ms`,
    );
    if (event.query.parameters && event.query.parameters.length > 0) {
      logger.debug({ params: event.query.parameters }, "Query parameters");
    }
  } else if (event.level === "error") {
    logger.error(
      {
        error: event.error,
        sql: event.query?.sql,
        params: event.query?.parameters,
      },
      "Query error",
    );
  }
};

export async function getDb(): Promise<Kysely<any>> {
  if (globalForDb.__INCONVO_KYSELY_DB__) {
    return globalForDb.__INCONVO_KYSELY_DB__;
  }

  let db: Kysely<any>;
  const isDevelopment = env.NODE_ENV === "development";

  if (env.DATABASE_DIALECT === "mysql") {
    const mysqlPool = createMysqlPool({
      uri: env.INCONVO_DATABASE_URL,
    });

    const dialect = new MysqlDialect({
      // mysql2 has a wider surface than the minimal subset Kysely declares,
      // so we cast to satisfy the dialect's structural typing expectations.
      pool: mysqlPool as unknown as MysqlDialectConfig["pool"],
    });
    db = new Kysely({
      dialect,
      log: isDevelopment ? createLogger() : undefined,
    });
  } else if (
    env.DATABASE_DIALECT === "postgresql" ||
    env.DATABASE_DIALECT === "redshift"
  ) {
    const connectionString = env.INCONVO_DATABASE_URL;
    const poolConfig: ConstructorParameters<typeof Pool>[0] = {
      connectionString,
    };

    const dialect = new PostgresDialect({
      pool: new Pool(poolConfig),
    });
    db = new Kysely({
      dialect,
      log: isDevelopment ? createLogger() : undefined,
    });
  } else if (env.DATABASE_DIALECT === "mssql") {
    // Parse MSSQL connection string
    const url = new URL(env.INCONVO_DATABASE_URL);
    const [username, password] =
      url.username && url.password
        ? [url.username, url.password]
        : url.pathname.slice(1).split(":");

    // Load tedious and tarn dynamically
    const tedious = require("tedious");
    const tarn = require("tarn");

    const dialect = new MssqlDialect({
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
          // Return an unconnected connection - Kysely will call connect() on it
          const connection = new tedious.Connection({
            server: url.hostname,
            authentication: {
              type: "default",
              options: {
                userName: decodeURIComponent(username || ""),
                password: decodeURIComponent(password || ""),
              },
            },
            options: {
              port: parseInt(url.port || "1433"),
              database:
                url.pathname.split("/")[1] ||
                url.searchParams.get("database") ||
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
    });

    db = new Kysely({
      dialect,
      log: isDevelopment ? createLogger() : undefined,
    });
  } else if (env.DATABASE_DIALECT === "bigquery") {
    const credentialsJson = env.INCONVO_BIGQUERY_CREDENTIALS_JSON?.trim();
    let parsedCredentials: Record<string, unknown> | undefined;

    if (credentialsJson) {
      try {
        parsedCredentials = JSON.parse(credentialsJson);
      } catch {
        throw new Error("Invalid INCONVO_BIGQUERY_CREDENTIALS_JSON value");
      }
    }

    const projectId = env.INCONVO_BIGQUERY_PROJECT_ID;
    const dataset = env.INCONVO_BIGQUERY_DATASET ?? env.INCONVO_DATABASE_SCHEMA;

    if (!projectId) {
      throw new Error("INCONVO_BIGQUERY_PROJECT_ID is required for BigQuery");
    }

    if (!dataset) {
      throw new Error(
        "INCONVO_BIGQUERY_DATASET or INCONVO_DATABASE_SCHEMA is required for BigQuery",
      );
    }

    const dialectConfig: BigQueryDialectConfig = {
      projectId,
      dataset,
      location: env.INCONVO_BIGQUERY_LOCATION,
      credentials: parsedCredentials,
      keyFilename: env.INCONVO_BIGQUERY_KEYFILE,
      maximumBytesBilled: env.INCONVO_BIGQUERY_MAX_BYTES_BILLED,
    };

    const dialect = new BigQueryDialect(dialectConfig);
    db = new Kysely({
      dialect,
      log: isDevelopment ? createLogger() : undefined,
    });
  } else {
    throw new Error(
      "Unsupported database dialect. Must be postgresql, redshift, mysql, mssql, or bigquery",
    );
  }

  globalForDb.__INCONVO_KYSELY_DB__ = db;
  return db;
}
