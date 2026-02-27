import { Command } from "commander";
import { execSync, spawn, spawnSync, type ChildProcess } from "child_process";
import * as fs from "fs";
import * as path from "path";
import * as os from "os";
import { fileURLToPath } from "url";
import * as p from "@clack/prompts";
import { envExists, runSetupWizard, readEnvFile } from "../wizard/setup.js";
import { logInfo, logError, logDim, COLORS } from "../process/output.js";
import { seedDemoData } from "../seed/demo-data.js";
import {
  ensureBinary,
  getCliVersion,
  checkPlatformSupport,
} from "../binary/manager.js";

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const INCONVO_DIR = path.join(os.homedir(), ".inconvo");
const DEMO_COMPOSE_FILE = path.join(INCONVO_DIR, "demo-db-compose.yml");
const INIT_SCRIPT = path.join(INCONVO_DIR, "demo-db-init.sql");
const DATA_DIR = path.join(INCONVO_DIR, "data");
const SANDBOX_DIR = path.join(INCONVO_DIR, "sandbox");
const SANDBOX_ENV_FILE = path.join(SANDBOX_DIR, ".dev.vars");

const SANDBOX_BASE_URL = "http://localhost:8787";
const SANDBOX_PORT = "8787";
const WRANGLER_VERSION = "4.61.1";

// Bundled files (shipped with npm package)
const BUNDLED_DEMO_COMPOSE_FILE = path.join(
  __dirname,
  "..",
  "..",
  "demo-db-compose.yml",
);
const BUNDLED_INIT_SCRIPT = path.join(
  __dirname,
  "..",
  "..",
  "demo-db-init.sql",
);
const BUNDLED_SANDBOX_DIR = path.join(
  __dirname,
  "..",
  "..",
  "assets",
  "sandbox",
);

function checkDockerRunning(): boolean {
  try {
    execSync("docker info", { stdio: "pipe" });
    return true;
  } catch {
    return false;
  }
}

function checkDockerComposeAvailable(): boolean {
  try {
    execSync("docker compose version", { stdio: "pipe" });
    return true;
  } catch {
    return false;
  }
}

function generateApiKey(): string {
  return crypto.randomUUID();
}

/**
 * Open a URL in the system's default browser
 */
function openBrowser(url: string): void {
  const platform = process.platform;
  const cmd =
    platform === "darwin"
      ? "open"
      : platform === "win32"
        ? "start"
        : "xdg-open";
  try {
    execSync(`${cmd} ${url}`, { stdio: "ignore" });
  } catch {
    // Silently fail if browser can't be opened
  }
}

/**
 * Wait for a URL to become available
 */
async function waitForServer(url: string, maxAttempts = 60): Promise<boolean> {
  for (let i = 0; i < maxAttempts; i++) {
    try {
      const controller = new AbortController();
      const timeout = setTimeout(() => controller.abort(), 2000);
      const response = await fetch(url, { signal: controller.signal });
      clearTimeout(timeout);
      if (response.ok || response.status < 500) {
        return true;
      }
    } catch {
      // Server not ready yet
    }
    await new Promise((resolve) => setTimeout(resolve, 1000));
  }
  return false;
}

/**
 * Wait for the demo database to be ready and seed it with data
 */
async function waitForDemoDbAndSeed(maxAttempts = 60): Promise<void> {
  const connectionString = "postgresql://inconvo:inconvo@localhost:26687/demo";

  for (let i = 0; i < maxAttempts; i++) {
    try {
      await seedDemoData(connectionString);
      return;
    } catch {
      // Database not ready yet
    }
    await new Promise((resolve) => setTimeout(resolve, 2000));
  }
  console.log(
    `${COLORS.dim}  Warning: Could not seed demo database${COLORS.reset}`,
  );
}

/**
 * Configure the dev-server's SQLite database for demo mode
 */
async function configureDemoDatabase(
  devServerUrl: string,
  maxAttempts = 30,
): Promise<void> {
  const setupUrl = `${devServerUrl}/api/demo/setup`;

  for (let i = 0; i < maxAttempts; i++) {
    try {
      const controller = new AbortController();
      const timeout = setTimeout(() => controller.abort(), 5000);
      const response = await fetch(setupUrl, {
        method: "POST",
        signal: controller.signal,
      });
      clearTimeout(timeout);

      if (response.ok) {
        logInfo("Demo database configured");
        return;
      }
    } catch {
      // Server not ready yet
    }
    await new Promise((resolve) => setTimeout(resolve, 2000));
  }
  console.log(
    `${COLORS.dim}  Warning: Could not configure demo database${COLORS.reset}`,
  );
}

function copyBundledFiles(useDemo: boolean): void {
  fs.mkdirSync(INCONVO_DIR, { recursive: true });
  fs.mkdirSync(DATA_DIR, { recursive: true });

  // Only copy Docker-related files in demo mode
  if (useDemo) {
    fs.copyFileSync(BUNDLED_DEMO_COMPOSE_FILE, DEMO_COMPOSE_FILE);
    fs.copyFileSync(BUNDLED_INIT_SCRIPT, INIT_SCRIPT);
  }

  if (!fs.existsSync(BUNDLED_SANDBOX_DIR)) {
    throw new Error(
      "Sandbox assets not found. Run `pnpm --filter inconvo build` first.",
    );
  }

  fs.rmSync(SANDBOX_DIR, { recursive: true, force: true });
  fs.cpSync(BUNDLED_SANDBOX_DIR, SANDBOX_DIR, { recursive: true });
}

function writeSandboxEnv(apiKey: string): void {
  const lines = [`INTERNAL_API_KEY=${apiKey}`, "SKIP_BUCKET_MOUNT=true", ""];
  fs.writeFileSync(SANDBOX_ENV_FILE, lines.join("\n"));
}

function pipeWithPrefix(stream: NodeJS.ReadableStream, prefix: string): void {
  let buffer = "";
  stream.on("data", (chunk) => {
    buffer += chunk.toString();
    const lines = buffer.split(/\r?\n/);
    buffer = lines.pop() ?? "";
    for (const line of lines) {
      if (line.trim() === "") continue;
      console.log(`${prefix} ${line}`);
    }
  });
  stream.on("end", () => {
    if (buffer.trim() !== "") {
      console.log(`${prefix} ${buffer}`);
    }
  });
}

function startSandbox(): ChildProcess {
  const npxCmd = process.platform === "win32" ? "npx.cmd" : "npx";
  const args = [
    "--yes",
    `wrangler@${WRANGLER_VERSION}`,
    "dev",
    "--config",
    "wrangler.jsonc",
    "--env",
    "dev",
    "--local",
    "--port",
    SANDBOX_PORT,
    "--ip",
    "127.0.0.1",
  ];

  const sandboxEnv: Record<string, string> = {
    ...process.env,
    WRANGLER_SEND_METRICS: "false",
  };
  for (const key of Object.keys(sandboxEnv)) {
    if (key.startsWith("npm_package_")) {
      delete sandboxEnv[key];
    }
  }
  delete sandboxEnv.npm_config_package;
  delete sandboxEnv.npm_config_argv;

  return spawn(npxCmd, args, {
    cwd: SANDBOX_DIR,
    env: sandboxEnv,
    stdio: ["ignore", "pipe", "pipe"],
  });
}

/**
 * Start the demo PostgreSQL database via Docker Compose.
 * Only used in demo mode — non-demo users never need Docker.
 */
function startDemoDb(env: Record<string, string>): void {
  const result = spawnSync(
    "docker",
    [
      "compose",
      "-f",
      DEMO_COMPOSE_FILE,
      "up",
      "--pull",
      "always",
      "--remove-orphans",
      "-d",
    ],
    { env, stdio: "inherit" },
  );

  if (result.error) {
    throw result.error;
  }
  if (result.status !== 0) {
    throw new Error(
      `Failed to start demo database (exit code ${result.status ?? "unknown"}).`,
    );
  }
}

/**
 * Stop the demo PostgreSQL database.
 */
function stopDemoDb(env: Record<string, string>): void {
  try {
    spawnSync("docker", ["compose", "-f", DEMO_COMPOSE_FILE, "down"], {
      stdio: "inherit",
      env,
    });
  } catch {
    // Ignore errors during shutdown
  }
}

function runBinaryMigrations(
  binaryPath: string,
  env: Record<string, string>,
): void {
  const result = spawnSync(binaryPath, ["--migrate-only"], {
    env,
    stdio: "inherit",
  });

  if (result.error) {
    throw result.error;
  }
  if (result.status !== 0) {
    throw new Error(
      `Migration command failed with exit code ${result.status ?? "unknown"}.`,
    );
  }
}

export const devCommand = new Command("dev")
  .description("Start the Inconvo dev server and sandbox")
  .option(
    "--binary-version <version>",
    "Use a specific dev-server binary version (default: CLI version)",
  )
  .action(async (options: { binaryVersion?: string }) => {
    // Check platform support
    try {
      checkPlatformSupport();
    } catch (error) {
      logError(error instanceof Error ? error.message : String(error));
      process.exit(1);
    }

    // Check for config, run wizard if missing
    if (!(await envExists())) {
      p.intro("Welcome to Inconvo! Let's set up your configuration first.");
      const success = await runSetupWizard();
      if (!success) {
        process.exit(1);
      }
    }

    // Read config from ~/.inconvo/config.env
    const configEnv = await readEnvFile();
    const useDemo = configEnv.USE_DEMO_DATABASE === "true";

    // Demo mode requires Docker for the PostgreSQL database
    if (useDemo) {
      if (!checkDockerRunning()) {
        logError(
          "Docker is required for demo mode. Please start Docker Desktop or Docker daemon.",
        );
        process.exit(1);
      }
      if (!checkDockerComposeAvailable()) {
        logError(
          "Docker Compose is required for demo mode. Please install Docker Desktop or docker-compose.",
        );
        process.exit(1);
      }
      logInfo("Docker is running (required for demo database)");
    }

    // Copy bundled files to ~/.inconvo/
    const spinner = p.spinner();
    spinner.start("Preparing local assets...");

    try {
      copyBundledFiles(useDemo);
      spinner.stop("Local assets ready");
    } catch (error) {
      spinner.stop("Failed to prepare local assets");
      logError(
        `Error: ${error instanceof Error ? error.message : String(error)}`,
      );
      process.exit(1);
    }

    // Ensure the compiled binary is available
    const binaryVersion = options.binaryVersion || getCliVersion();
    spinner.start("Checking dev-server binary...");

    let binaryPath: string;
    try {
      binaryPath = await ensureBinary(binaryVersion, (msg) => {
        spinner.message(msg);
      });
      spinner.stop("Dev-server binary ready");
    } catch (error) {
      spinner.stop("Failed to get dev-server binary");
      logError(
        `Error: ${error instanceof Error ? error.message : String(error)}`,
      );
      process.exit(1);
    }

    // Generate shared API key for internal communication
    const sandboxApiKey = generateApiKey();

    // Build environment for the dev-server binary
    // No need for rewriteLocalhostForDocker — binary runs on host directly
    const serverEnv: Record<string, string> = {
      ...(process.env as Record<string, string>),
      ...configEnv,
      INCONVO_LOCAL_DB_PATH: path.join(DATA_DIR, "inconvo.db"),
      INCONVO_SANDBOX_API_KEY: sandboxApiKey,
      INCONVO_SANDBOX_BASE_URL: SANDBOX_BASE_URL,
      SKIP_ENV_VALIDATION: "true",
      PORT: "26686",
      HOSTNAME: "0.0.0.0",
      NODE_ENV: "production",
    };

    // Set demo database URL when demo mode is enabled
    // Uses localhost since the binary runs on the host (not in Docker)
    if (useDemo) {
      serverEnv.DATABASE_DIALECT = "postgresql";
      serverEnv.INCONVO_DATABASE_URL =
        "postgresql://inconvo:inconvo@localhost:26687/demo";
    }

    spinner.start("Running local database migrations...");
    try {
      runBinaryMigrations(binaryPath, serverEnv);
      spinner.stop("Local database ready");
    } catch (error) {
      spinner.stop("Failed to run local migrations");
      logError(
        `Error: ${error instanceof Error ? error.message : String(error)}`,
      );
      process.exit(1);
    }

    writeSandboxEnv(sandboxApiKey);

    const devServerUrl = "http://localhost:26686";
    const telemetryDisabled = configEnv.DISABLE_TELEMETRY === "true";

    // Print startup banner
    console.log("");
    console.log(`${COLORS.cyan}${COLORS.bold}  Inconvo${COLORS.reset}`);
    console.log("");
    if (telemetryDisabled) {
      console.log(`${COLORS.dim}  Telemetry disabled${COLORS.reset}`);
    } else {
      console.log(
        `${COLORS.dim}  Anonymous telemetry is enabled to help improve Inconvo.${COLORS.reset}`,
      );
      console.log(
        `${COLORS.dim}  Run 'inconvo telemetry off' to disable.${COLORS.reset}`,
      );
    }
    console.log("");
    logDim(`  dev-server: ${devServerUrl}`);
    logDim(`  sandbox:    http://localhost:${SANDBOX_PORT}`);
    logDim("  Press Ctrl+C to stop\n");

    // Start demo database if in demo mode
    if (useDemo) {
      logDim("Starting demo database...");
      try {
        startDemoDb(process.env as Record<string, string>);
      } catch (error) {
        logError(
          `Failed to start demo database: ${error instanceof Error ? error.message : String(error)}`,
        );
        process.exit(1);
      }
    }

    // Start the dev-server binary directly (no Docker needed)
    logDim("Starting dev-server...");
    const serverProc = spawn(binaryPath, [], {
      env: serverEnv,
      stdio: ["ignore", "pipe", "pipe"],
    });

    const sandboxProc = startSandbox();

    pipeWithPrefix(
      serverProc.stdout!,
      `${COLORS.cyan}[dev-server]${COLORS.reset}`,
    );
    pipeWithPrefix(
      serverProc.stderr!,
      `${COLORS.cyan}[dev-server]${COLORS.reset}`,
    );
    pipeWithPrefix(
      sandboxProc.stdout!,
      `${COLORS.yellow}[sandbox]${COLORS.reset}`,
    );
    pipeWithPrefix(
      sandboxProc.stderr!,
      `${COLORS.yellow}[sandbox]${COLORS.reset}`,
    );

    let shuttingDown = false;

    const shutdown = (code = 0) => {
      if (shuttingDown) return;
      shuttingDown = true;

      console.log(`\n${COLORS.gray}Shutting down...${COLORS.reset}`);

      if (serverProc && !serverProc.killed) {
        serverProc.kill("SIGTERM");
      }
      if (sandboxProc && !sandboxProc.killed) {
        sandboxProc.kill("SIGINT");
      }
      if (useDemo) {
        stopDemoDb(process.env as Record<string, string>);
      }

      process.exit(code);
    };

    process.on("SIGINT", () => shutdown(0));
    process.on("SIGTERM", () => shutdown(0));

    serverProc.on("error", (error) => {
      logError(`Failed to start dev-server: ${error.message}`);
      shutdown(1);
    });

    serverProc.on("exit", (code) => {
      if (!shuttingDown) {
        logError("Dev-server exited unexpectedly.");
        shutdown(code ?? 1);
      }
    });

    sandboxProc.on("error", (error) => {
      logError(`Failed to start sandbox: ${error.message}`);
      shutdown(1);
    });

    sandboxProc.on("exit", (code) => {
      if (!shuttingDown) {
        logError("Sandbox exited unexpectedly.");
        shutdown(code ?? 1);
      }
    });

    // Wait for services to be ready
    if (useDemo) {
      (async () => {
        await waitForDemoDbAndSeed();
        const serverReady = await waitForServer(devServerUrl);
        if (serverReady) {
          await configureDemoDatabase(devServerUrl);
          logInfo("Opening browser...");
          openBrowser(devServerUrl);
        }
      })().catch(() => {
        // Silently ignore errors
      });
    } else {
      waitForServer(devServerUrl)
        .then((ready) => {
          if (ready) {
            logInfo("Opening browser...");
            openBrowser(devServerUrl);
          }
        })
        .catch(() => {
          // Silently ignore errors
        });
    }
  });
