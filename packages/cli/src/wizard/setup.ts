import * as p from "@clack/prompts";
import * as fs from "fs/promises";
import * as path from "path";
import * as os from "os";
import { Kysely, PostgresDialect, MysqlDialect, MssqlDialect } from "kysely";
import { Pool } from "pg";
import { createPool as createMysqlPool } from "mysql2";

type SqlDialect = "postgresql" | "redshift" | "mysql" | "mssql";
export type DatabaseDialect = SqlDialect | "bigquery";

interface SqlDatabaseConfig {
  databaseDialect: SqlDialect;
  databaseUrl: string;
  databaseSchemas?: string[];
}

interface BigQueryDatabaseConfig {
  databaseDialect: "bigquery";
  bigQueryProjectId: string;
  bigQueryDataset: string;
  bigQueryLocation: string;
  bigQueryKeyfile: string;
  bigQueryMaxBytesBilled?: number;
}

type DatabaseConfig = SqlDatabaseConfig | BigQueryDatabaseConfig;

type SetupConfig = DatabaseConfig & {
  openaiApiKey: string;
  useDemo: boolean;
};

const BIGQUERY_CREDENTIALS_FILENAME = "bigquery-service-account.json";
const BIGQUERY_CREDENTIALS_CONTAINER_PATH =
  `/inconvo/credentials/${BIGQUERY_CREDENTIALS_FILENAME}`;

function getBigQueryCredentialsDir(): string {
  return path.join(getInconvoDir(), "credentials");
}

function getBigQueryCredentialsHostPath(): string {
  return path.join(getBigQueryCredentialsDir(), BIGQUERY_CREDENTIALS_FILENAME);
}

async function testSqlDatabaseConnection(
  dialect: SqlDialect,
  connectionUrl: string
): Promise<void> {
  let db: Kysely<unknown>;

  if (dialect === "postgresql" || dialect === "redshift") {
    db = new Kysely({
      dialect: new PostgresDialect({
        pool: new Pool({ connectionString: connectionUrl }),
      }),
    });
  } else if (dialect === "mysql") {
    const mysqlPool = createMysqlPool({ uri: connectionUrl });
    db = new Kysely({
      dialect: new MysqlDialect({
        pool: mysqlPool as never,
      }),
    });
  } else if (dialect === "mssql") {
    const url = new URL(connectionUrl);
    const [username, password] =
      url.username && url.password
        ? [url.username, url.password]
        : url.pathname.slice(1).split(":");

    /* eslint-disable @typescript-eslint/no-require-imports */
    const tedious = require("tedious");
    const tarn = require("tarn");
    /* eslint-enable @typescript-eslint/no-require-imports */

    db = new Kysely({
      dialect: new MssqlDialect({
        tarn: {
          ...tarn,
          options: {
            min: 0,
            max: 1,
            acquireTimeoutMillis: 10000,
            createTimeoutMillis: 10000,
            destroyTimeoutMillis: 5000,
            idleTimeoutMillis: 10000,
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
                connectTimeout: 10000,
              },
            });
            return connection;
          },
        },
      }),
    });
  } else {
    const _exhaustiveCheck: never = dialect;
    throw new Error(`Unsupported dialect: ${String(_exhaustiveCheck)}`);
  }

  try {
    await db
      .selectFrom("information_schema.tables" as never)
      .limit(1)
      .execute();
  } finally {
    await db.destroy();
  }
}

async function testBigQueryConnection(
  config: BigQueryDatabaseConfig
): Promise<void> {
  const { BigQuery } = await import("@google-cloud/bigquery");
  const client = new BigQuery({
    projectId: config.bigQueryProjectId,
    keyFilename: getBigQueryCredentialsHostPath(),
  });

  const datasetPath = `\`${config.bigQueryProjectId}.${config.bigQueryDataset}\``;

  await client.query({
    query: `SELECT table_name FROM ${datasetPath}.INFORMATION_SCHEMA.TABLES LIMIT 1`,
    useLegacySql: false,
    location: config.bigQueryLocation,
  });
}

async function testDatabaseConnection(config: DatabaseConfig): Promise<void> {
  if (config.databaseDialect === "bigquery") {
    await testBigQueryConnection(config);
    return;
  }

  await testSqlDatabaseConnection(config.databaseDialect, config.databaseUrl);
}

async function persistBigQueryCredentialsFile(sourcePath: string): Promise<void> {
  let raw: string;

  try {
    raw = await fs.readFile(sourcePath, "utf-8");
  } catch {
    throw new Error("Could not read the service account JSON file");
  }

  let parsed: unknown;
  try {
    parsed = JSON.parse(raw);
  } catch {
    throw new Error("Service account file is not valid JSON");
  }

  if (!parsed || typeof parsed !== "object" || Array.isArray(parsed)) {
    throw new Error("Service account file must be a JSON object");
  }

  await fs.mkdir(getBigQueryCredentialsDir(), { recursive: true });
  const destinationPath = getBigQueryCredentialsHostPath();
  await fs.writeFile(destinationPath, JSON.stringify(parsed), { mode: 0o600 });
  try {
    await fs.chmod(destinationPath, 0o600);
  } catch {
    // chmod may fail on some filesystems/platforms; ignore.
  }
}

async function writeEnvFile(
  config: SetupConfig,
  envPath: string
): Promise<void> {
  const localSecretKey = `local-dev-${crypto.randomUUID()}`;

  const lines: string[] = [
    "# Inconvo Dev Server Configuration",
    "# Generated by inconvo configure",
    "",
    "# Database Configuration",
    `DATABASE_DIALECT=${config.databaseDialect}`,
  ];

  if (config.databaseDialect === "bigquery") {
    lines.push(`INCONVO_BIGQUERY_PROJECT_ID=${config.bigQueryProjectId}`);
    lines.push(`INCONVO_BIGQUERY_DATASET=${config.bigQueryDataset}`);
    lines.push(`INCONVO_BIGQUERY_LOCATION=${config.bigQueryLocation}`);
    lines.push(`INCONVO_BIGQUERY_KEYFILE=${config.bigQueryKeyfile}`);
    if (config.bigQueryMaxBytesBilled !== undefined) {
      lines.push(
        `INCONVO_BIGQUERY_MAX_BYTES_BILLED=${config.bigQueryMaxBytesBilled}`
      );
    }
  } else {
    lines.push(`INCONVO_DATABASE_URL=${config.databaseUrl}`);
    if (config.databaseSchemas?.length) {
      lines.push(`INCONVO_DATABASE_SCHEMA=${config.databaseSchemas.join(",")}`);
    }
  }

  lines.push(
    "",
    "# Connect Configuration (for local query execution)",
    `INCONVO_SECRET_KEY=${localSecretKey}`,
    "",
    "# LLM Configuration",
    `OPENAI_API_KEY=${config.openaiApiKey}`,
    "",
    "# Demo Mode",
    `USE_DEMO_DATABASE=${config.useDemo ? "true" : "false"}`,
    "",
    "# Telemetry (anonymous usage data to help improve Inconvo)",
    "# Run 'inconvo telemetry off' to disable",
    "DISABLE_TELEMETRY=false",
    ""
  );

  await fs.writeFile(envPath, lines.join("\n"));
}

export function getInconvoDir(): string {
  return path.join(os.homedir(), ".inconvo");
}

export function getEnvPath(): string {
  return path.join(getInconvoDir(), "config.env");
}

export async function envExists(): Promise<boolean> {
  try {
    await fs.access(getEnvPath());
    return true;
  } catch {
    return false;
  }
}

export async function ensureInconvoDir(): Promise<void> {
  const dir = getInconvoDir();
  await fs.mkdir(dir, { recursive: true });
}

export async function readEnvFile(): Promise<Record<string, string>> {
  const envPath = getEnvPath();
  try {
    const content = await fs.readFile(envPath, "utf-8");
    const env: Record<string, string> = {};
    for (const line of content.split("\n")) {
      const trimmed = line.trim();
      if (trimmed && !trimmed.startsWith("#")) {
        const [key, ...valueParts] = trimmed.split("=");
        if (key) {
          env[key] = valueParts.join("=");
        }
      }
    }
    return env;
  } catch {
    return {};
  }
}

const maskSecret = (value: string, visibleChars = 20): string => {
  if (value.length <= visibleChars) return value;
  return `${value.slice(0, visibleChars)}${"*".repeat(value.length - visibleChars)}`;
};

export async function runSetupWizard(): Promise<boolean> {
  p.intro("Inconvo Configuration Setup");

  const envPath = getEnvPath();

  // Ensure ~/.inconvo directory exists
  await ensureInconvoDir();

  // Check if config already exists
  if (await envExists()) {
    const shouldOverwrite = await p.confirm({
      message: `Configuration already exists at ${envPath}. Do you want to overwrite it?`,
      initialValue: false,
    });

    if (p.isCancel(shouldOverwrite) || !shouldOverwrite) {
      p.cancel("Setup cancelled. Existing configuration preserved.");
      return false;
    }
  }

  // Database source selection
  const databaseSource = await p.select({
    message: "Choose your database:",
    options: [
      {
        value: "demo",
        label: "Demo database",
        hint: "PostgreSQL in Docker - great for trying out Inconvo",
      },
      {
        value: "own",
        label: "My own database",
        hint: "Connect to your existing database",
      },
    ],
  });

  if (p.isCancel(databaseSource)) {
    p.cancel("Setup cancelled.");
    return false;
  }

  const useDemo = databaseSource === "demo";
  let databaseConfig: DatabaseConfig;

  if (useDemo) {
    // Demo database configuration - uses Docker container
    databaseConfig = {
      databaseDialect: "postgresql",
      databaseUrl: "postgresql://inconvo:inconvo@demo-db:5432/demo",
      databaseSchemas: ["public"],
    };
    p.log.info("Demo database will be started in a Docker container.");
  } else {
    // User's own database
    const dialectChoice = await p.select({
      message: "Select your database type:",
      options: [
        { value: "postgresql", label: "PostgreSQL", hint: "recommended" },
        { value: "mssql", label: "Microsoft SQL Server" },
        { value: "mysql", label: "MySQL" },
        { value: "redshift", label: "Amazon Redshift" },
        { value: "bigquery", label: "Google BigQuery" },
      ],
    });

    if (p.isCancel(dialectChoice)) {
      p.cancel("Setup cancelled.");
      return false;
    }

    const databaseDialect = dialectChoice as DatabaseDialect;

    if (databaseDialect === "bigquery") {
      const projectIdInput = await p.text({
        message: "BigQuery project ID:",
        placeholder: "my-gcp-project",
        validate: (value) => (value ? undefined : "Project ID is required"),
      });
      if (p.isCancel(projectIdInput)) {
        p.cancel("Setup cancelled.");
        return false;
      }

      const datasetInput = await p.text({
        message: "BigQuery dataset:",
        placeholder: "analytics",
        validate: (value) => (value ? undefined : "Dataset is required"),
      });
      if (p.isCancel(datasetInput)) {
        p.cancel("Setup cancelled.");
        return false;
      }

      const locationInput = await p.text({
        message: "BigQuery location (for example US or EU):",
        placeholder: "US",
        validate: (value) => (value ? undefined : "Location is required"),
      });
      if (p.isCancel(locationInput)) {
        p.cancel("Setup cancelled.");
        return false;
      }

      while (true) {
        const credentialsPath = await p.text({
          message: "Path to service account JSON key file:",
          placeholder: "/Users/you/keys/bigquery-service-account.json",
          validate: (value) => (value ? undefined : "Path is required"),
        });
        if (p.isCancel(credentialsPath)) {
          p.cancel("Setup cancelled.");
          return false;
        }

        try {
          await persistBigQueryCredentialsFile(credentialsPath.trim());
          p.log.info(
            `Credentials copied to ${getBigQueryCredentialsHostPath()}`
          );
          break;
        } catch (error) {
          p.log.error(
            error instanceof Error ? error.message : "Invalid credentials file"
          );
          const retryFile = await p.confirm({
            message: "Try a different file path?",
            initialValue: true,
          });
          if (p.isCancel(retryFile) || !retryFile) {
            p.cancel("Setup cancelled.");
            return false;
          }
        }
      }

      const maxBytesInput = await p.text({
        message: "Max bytes billed per query (optional, press Enter to skip):",
        placeholder: "1000000000",
        validate: (value) => {
          if (!value) return undefined;
          return /^\d+$/.test(value)
            ? undefined
            : "Must be a positive integer";
        },
      });
      if (p.isCancel(maxBytesInput)) {
        p.cancel("Setup cancelled.");
        return false;
      }

      databaseConfig = {
        databaseDialect,
        bigQueryProjectId: projectIdInput.trim(),
        bigQueryDataset: datasetInput.trim(),
        bigQueryLocation: locationInput.trim(),
        bigQueryKeyfile: BIGQUERY_CREDENTIALS_CONTAINER_PATH,
        bigQueryMaxBytesBilled: maxBytesInput
          ? Number.parseInt(maxBytesInput, 10)
          : undefined,
      };
    } else {
      // Connection string
      const placeholders: Record<SqlDialect, string> = {
        postgresql: "postgresql://user:password@localhost:5432/database",
        redshift:
          "postgresql://user:password@cluster.region.redshift.amazonaws.com:5439/database?sslmode=require",
        mysql: "mysql://user:password@localhost:3306/database",
        mssql:
          "mssql://user:password@localhost:1433/database?encrypt=true&trustServerCertificate=true",
      };

      const urlInput = await p.text({
        message: "Enter your database connection string:",
        placeholder: placeholders[databaseDialect],
        validate: (value) => {
          if (!value) return "Connection string is required";
          try {
            new URL(value);
            return undefined;
          } catch {
            return "Please enter a valid connection URL";
          }
        },
      });

      if (p.isCancel(urlInput)) {
        p.cancel("Setup cancelled.");
        return false;
      }

      // Database schemas (optional, comma-separated)
      const schemaInput = await p.text({
        message:
          "Database schemas (optional, comma-separated, press Enter to skip):",
        placeholder:
          databaseDialect === "postgresql" || databaseDialect === "redshift"
            ? "public, sales"
            : "",
      });

      if (p.isCancel(schemaInput)) {
        p.cancel("Setup cancelled.");
        return false;
      }

      databaseConfig = {
        databaseDialect,
        databaseUrl: urlInput,
        databaseSchemas: schemaInput
          ? schemaInput.split(",").map((s) => s.trim()).filter(Boolean)
          : undefined,
      };
    }

    // Test database connection
    const spinner = p.spinner();
    spinner.start("Testing database connection...");

    try {
      await testDatabaseConnection(databaseConfig);
      spinner.stop("Database connection successful!");
    } catch (error) {
      spinner.stop("Database connection failed");
      p.log.error(
        `Connection error: ${error instanceof Error ? error.message : String(error)}`
      );

      const retry = await p.confirm({
        message: "Would you like to try setup again?",
        initialValue: true,
      });

      if (p.isCancel(retry) || !retry) {
        p.cancel("Setup cancelled.");
        return false;
      }

      return runSetupWizard();
    }
  }

  // OpenAI API key
  const openaiApiKey = await p.password({
    message: "Enter your OpenAI API key:",
    mask: "*",
    clearOnError: true,
    validate: (value) => {
      if (!value) return "OpenAI API key is required";
      if (!value.startsWith("sk-")) return "Invalid OpenAI API key format";
      return undefined;
    },
  });

  if (p.isCancel(openaiApiKey)) {
    p.cancel("Setup cancelled.");
    return false;
  }
  const openaiApiKeyValue = openaiApiKey as string;

  // Write configuration
  const saveSpinner = p.spinner();
  saveSpinner.start("Saving configuration...");

  try {
    await writeEnvFile(
      {
        ...databaseConfig,
        openaiApiKey: openaiApiKeyValue,
        useDemo,
      },
      envPath
    );
    saveSpinner.stop(`Configuration saved to ${envPath}`);
    p.log.info(`OpenAI API key saved as ${maskSecret(openaiApiKeyValue)}`);
  } catch (error) {
    saveSpinner.stop("Failed to save configuration");
    p.log.error(
      `Error: ${error instanceof Error ? error.message : String(error)}`
    );
    return false;
  }

  p.outro("Setup complete! Run `inconvo dev` to start the dev server.");

  return true;
}
