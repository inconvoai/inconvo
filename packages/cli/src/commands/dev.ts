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

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const INCONVO_DIR = path.join(os.homedir(), ".inconvo");
const COMPOSE_FILE = path.join(INCONVO_DIR, "docker-compose.yml");
const INIT_SCRIPT = path.join(INCONVO_DIR, "demo-db-init.sql");
const DATA_DIR = path.join(INCONVO_DIR, "data");
const SANDBOX_DIR = path.join(INCONVO_DIR, "sandbox");
const SANDBOX_ENV_FILE = path.join(SANDBOX_DIR, ".dev.vars");

const SANDBOX_BASE_URL = "http://host.docker.internal:8787";
const SANDBOX_PORT = "8787";
const WRANGLER_VERSION = "4.61.1";

// Bundled files (shipped with npm package)
const BUNDLED_COMPOSE_FILE = path.join(
  __dirname,
  "..",
  "..",
  "docker-compose.yml",
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
 * Rewrite localhost URLs to host.docker.internal for Docker access to host services.
 * This allows users to enter localhost URLs (which the CLI can validate) while
 * Docker containers can still reach host services.
 */
function rewriteLocalhostForDocker(url: string): string {
  return url
    .replace(/localhost/gi, "host.docker.internal")
    .replace(/127\.0\.0\.1/g, "host.docker.internal");
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

  // Wait for database to be ready
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
 * This sets up user context, computed columns, and table conditions
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

function copyBundledFiles(): void {
  // Copy the bundled files to ~/.inconvo/
  // This ensures the file versions match the CLI version
  fs.mkdirSync(INCONVO_DIR, { recursive: true });
  fs.mkdirSync(DATA_DIR, { recursive: true });
  fs.copyFileSync(BUNDLED_COMPOSE_FILE, COMPOSE_FILE);
  fs.copyFileSync(BUNDLED_INIT_SCRIPT, INIT_SCRIPT);

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

function runCommand(
  command: string,
  args: string[],
  options: {
    env?: NodeJS.ProcessEnv;
    cwd?: string;
    stdio?: "inherit" | "pipe";
  },
): Promise<void> {
  return new Promise((resolve, reject) => {
    const proc = spawn(command, args, {
      env: options.env,
      cwd: options.cwd,
      stdio: options.stdio ?? "inherit",
    });

    proc.on("error", (error) => reject(error));
    proc.on("exit", (code) => {
      if (code === 0) {
        resolve();
      } else {
        reject(new Error(`${command} exited with code ${code ?? "unknown"}`));
      }
    });
  });
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

function startDockerLogs(
  env: NodeJS.ProcessEnv,
  useDemo: boolean,
): ChildProcess {
  const args = ["compose", "-f", COMPOSE_FILE];
  if (useDemo) {
    args.push("--profile", "demo");
  }
  args.push("logs", "-f", "--no-color", "dev-server");

  return spawn("docker", args, {
    env,
    stdio: ["ignore", "pipe", "pipe"],
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

export const devCommand = new Command("dev")
  .description("Start the Inconvo dev server and sandbox")
  .option(
    "--image-version <version>",
    "Use a specific Docker image version (default: latest)",
  )
  .action(async (options: { imageVersion?: string }) => {
    const imageVersion = options.imageVersion || "latest";

    // Check Docker is running
    if (!checkDockerRunning()) {
      logError(
        "Docker is not running. Please start Docker Desktop or Docker daemon.",
      );
      process.exit(1);
    }

    // Check docker compose is available
    if (!checkDockerComposeAvailable()) {
      logError(
        "Docker Compose is not available. Please install Docker Desktop or docker-compose.",
      );
      process.exit(1);
    }

    logInfo("Docker is running");

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

    // Copy bundled files to ~/.inconvo/
    const spinner = p.spinner();
    spinner.start("Preparing local assets...");

    try {
      copyBundledFiles();
      spinner.stop("Local assets ready");
    } catch (error) {
      spinner.stop("Failed to prepare local assets");
      logError(
        `Error: ${error instanceof Error ? error.message : String(error)}`,
      );
      process.exit(1);
    }

    // Generate shared API key for internal communication
    const sandboxApiKey = generateApiKey();

    // Check if demo mode is enabled
    const useDemo = configEnv.USE_DEMO_DATABASE === "true";

    // Rewrite localhost to host.docker.internal for Docker access to host services
    // (not needed for demo mode since the database is in the same Docker network)
    const dockerEnv = { ...configEnv };
    if (!useDemo && dockerEnv.INCONVO_DATABASE_URL) {
      dockerEnv.INCONVO_DATABASE_URL = rewriteLocalhostForDocker(
        dockerEnv.INCONVO_DATABASE_URL,
      );
    }

    // Build environment for docker compose
    const env: Record<string, string> = {
      ...(process.env as Record<string, string>),
      ...dockerEnv,
      INCONVO_VERSION: imageVersion,
      INCONVO_SANDBOX_API_KEY: sandboxApiKey,
      INCONVO_SANDBOX_BASE_URL: SANDBOX_BASE_URL,
      INCONVO_INIT_SCRIPT: INIT_SCRIPT,
      INCONVO_DATA_DIR: DATA_DIR,
    };

    // Set demo database URL when demo mode is enabled
    if (useDemo) {
      env.DATABASE_DIALECT = "postgresql";
      env.INCONVO_DATABASE_URL =
        "postgresql://inconvo:inconvo@demo-db:5432/demo";
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

    logDim("Starting Docker services...");
    try {
      const composeArgs = ["compose", "-f", COMPOSE_FILE];
      if (useDemo) {
        composeArgs.push("--profile", "demo");
      }
      composeArgs.push("up", "--pull", "always", "--remove-orphans", "-d");
      await runCommand("docker", composeArgs, { env, stdio: "inherit" });
    } catch (error) {
      logError(
        `Failed to start Docker services: ${error instanceof Error ? error.message : String(error)}`,
      );
      process.exit(1);
    }

    const dockerLogsProc = startDockerLogs(env, useDemo);
    const sandboxProc = startSandbox();

    pipeWithPrefix(
      dockerLogsProc.stdout!,
      `${COLORS.cyan}[dev-server]${COLORS.reset}`,
    );
    pipeWithPrefix(
      dockerLogsProc.stderr!,
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

      if (sandboxProc && !sandboxProc.killed) {
        sandboxProc.kill("SIGINT");
      }
      if (dockerLogsProc && !dockerLogsProc.killed) {
        dockerLogsProc.kill("SIGTERM");
      }

      try {
        const downArgs = ["compose", "-f", COMPOSE_FILE];
        if (useDemo) {
          downArgs.push("--profile", "demo");
        }
        downArgs.push("down");
        spawnSync("docker", downArgs, {
          stdio: "inherit",
          env,
        });
      } catch {
        // Ignore errors during shutdown
      }

      process.exit(code);
    };

    process.on("SIGINT", () => shutdown(0));
    process.on("SIGTERM", () => shutdown(0));

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

    dockerLogsProc.on("error", (error) => {
      logError(`Failed to stream dev-server logs: ${error.message}`);
      shutdown(1);
    });

    dockerLogsProc.on("exit", (code) => {
      if (!shuttingDown) {
        logError("Dev-server logs stopped unexpectedly.");
        shutdown(code ?? 1);
      }
    });

    // Wait for demo database to be ready, seed it, and configure the dev-server (runs in background)
    if (useDemo) {
      (async () => {
        // First seed the demo database
        await waitForDemoDbAndSeed();
        // Then wait for dev-server to be ready
        const serverReady = await waitForServer(devServerUrl);
        if (serverReady) {
          // Configure the dev-server's SQLite database for demo mode
          await configureDemoDatabase(devServerUrl);
          logInfo("Opening browser...");
          openBrowser(devServerUrl);
        }
      })().catch(() => {
        // Silently ignore errors
      });
    } else {
      // Non-demo mode: just wait for server and open browser
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
