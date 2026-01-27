import * as p from "@clack/prompts";
import { Kysely, PostgresDialect, MysqlDialect, MssqlDialect } from "kysely";
import { Pool } from "pg";
import { createPool as createMysqlPool } from "mysql2";
import {
  configExists,
  loadConfig,
  saveConfig,
  generateSecrets,
  getConfigPath,
} from "../config/manager.js";
import type { DatabaseDialect, InconvoConfig } from "../config/schema.js";

async function testDatabaseConnection(
  dialect: DatabaseDialect,
  connectionUrl: string
): Promise<void> {
  let db: Kysely<unknown>;

  if (dialect === "postgresql") {
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

    /* eslint-disable @typescript-eslint/no-require-imports, @typescript-eslint/no-unsafe-assignment, @typescript-eslint/no-unsafe-member-access, @typescript-eslint/no-unsafe-call, @typescript-eslint/no-unsafe-return */
    const tedious = require("tedious");
    const tarn = require("tarn");

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
    /* eslint-enable @typescript-eslint/no-require-imports, @typescript-eslint/no-unsafe-assignment, @typescript-eslint/no-unsafe-member-access, @typescript-eslint/no-unsafe-call, @typescript-eslint/no-unsafe-return */
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

export async function runSetupWizard(): Promise<InconvoConfig | null> {
  p.intro("Inconvo Configuration Setup");

  // Check if config already exists
  if (await configExists()) {
    const existingConfig = await loadConfig();
    const shouldOverwrite = await p.confirm({
      message: `Configuration already exists at ${getConfigPath()}. Do you want to overwrite it?`,
      initialValue: false,
    });

    if (p.isCancel(shouldOverwrite) || !shouldOverwrite) {
      p.cancel("Setup cancelled. Existing configuration preserved.");
      return existingConfig;
    }
  }

  // Database type selection
  const databaseDialect = await p.select({
    message: "Select your database type:",
    options: [
      { value: "postgresql", label: "PostgreSQL", hint: "recommended" },
      { value: "mysql", label: "MySQL" },
      { value: "mssql", label: "Microsoft SQL Server" },
    ],
  });

  if (p.isCancel(databaseDialect)) {
    p.cancel("Setup cancelled.");
    return null;
  }

  // Connection string
  const placeholders: Record<DatabaseDialect, string> = {
    postgresql: "postgresql://user:password@localhost:5432/database",
    mysql: "mysql://user:password@localhost:3306/database",
    mssql: "mssql://user:password@localhost:1433/database",
  };

  const databaseUrl = await p.text({
    message: "Enter your database connection string:",
    placeholder: placeholders[databaseDialect as DatabaseDialect],
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

  if (p.isCancel(databaseUrl)) {
    p.cancel("Setup cancelled.");
    return null;
  }

  // Database schema (optional)
  const databaseSchema = await p.text({
    message: "Database schema (optional, press Enter to skip):",
    placeholder: databaseDialect === "postgresql" ? "public" : "",
  });

  if (p.isCancel(databaseSchema)) {
    p.cancel("Setup cancelled.");
    return null;
  }

  // Test database connection
  const spinner = p.spinner();
  spinner.start("Testing database connection...");

  try {
    await testDatabaseConnection(
      databaseDialect as DatabaseDialect,
      databaseUrl
    );
    spinner.stop("Database connection successful!");
  } catch (error) {
    spinner.stop("Database connection failed");
    p.log.error(
      `Connection error: ${error instanceof Error ? error.message : String(error)}`
    );

    const retry = await p.confirm({
      message: "Would you like to enter a different connection string?",
      initialValue: true,
    });

    if (p.isCancel(retry) || !retry) {
      p.cancel("Setup cancelled.");
      return null;
    }

    return runSetupWizard();
  }

  // OpenAI API key
  const openaiApiKey = await p.text({
    message: "Enter your OpenAI API key:",
    placeholder: "sk-...",
    validate: (value) => {
      if (!value) return "OpenAI API key is required";
      if (!value.startsWith("sk-")) return "Invalid OpenAI API key format";
      return undefined;
    },
  });

  if (p.isCancel(openaiApiKey)) {
    p.cancel("Setup cancelled.");
    return null;
  }

  // LangChain API key
  const langchainApiKey = await p.text({
    message: "Enter your LangChain API key (for prompts):",
    placeholder: "lsv2_pt_...",
    validate: (value) => {
      if (!value) return "LangChain API key is required";
      return undefined;
    },
  });

  if (p.isCancel(langchainApiKey)) {
    p.cancel("Setup cancelled.");
    return null;
  }

  // Generate secrets
  const secrets = generateSecrets();

  // Build config object
  const config: InconvoConfig = {
    version: 1,
    database: {
      dialect: databaseDialect as DatabaseDialect,
      url: databaseUrl,
      ...(databaseSchema && { schema: databaseSchema }),
    },
    apiKeys: {
      openai: openaiApiKey,
      langchain: langchainApiKey,
    },
    secrets,
  };

  // Save configuration
  spinner.start("Saving configuration...");

  try {
    await saveConfig(config);
    spinner.stop(`Configuration saved to ${getConfigPath()}`);
  } catch (error) {
    spinner.stop("Failed to save configuration");
    p.log.error(
      `Error: ${error instanceof Error ? error.message : String(error)}`
    );
    return null;
  }

  p.outro("Setup complete! Run `inconvo dev` to start the development environment.");

  return config;
}
