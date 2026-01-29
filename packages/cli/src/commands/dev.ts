import { Command } from "commander";
import { execSync, spawn } from "child_process";
import * as fs from "fs";
import * as path from "path";
import * as os from "os";
import * as p from "@clack/prompts";
import { envExists, runSetupWizard, readEnvFile } from "../wizard/setup.js";
import { logInfo, logError, logGray } from "../process/output.js";

const INCONVO_DIR = path.join(os.homedir(), ".inconvo");
const COMPOSE_FILE = path.join(INCONVO_DIR, "docker-compose.yml");
const GITHUB_RAW_BASE = "https://raw.githubusercontent.com/ten-dev/inconvo";

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

async function downloadComposeFile(version: string): Promise<void> {
  const branch = version === "latest" ? "main" : `v${version}`;
  const url = `${GITHUB_RAW_BASE}/${branch}/docker/docker-compose.yml`;

  const response = await fetch(url);
  if (!response.ok) {
    throw new Error(`Failed to download docker-compose.yml: ${response.status} ${response.statusText}`);
  }

  const content = await response.text();
  fs.mkdirSync(INCONVO_DIR, { recursive: true });
  fs.writeFileSync(COMPOSE_FILE, content);
}

export const devCommand = new Command("dev")
  .description("Start the Inconvo dev server and sandbox")
  .option("--version <version>", "Use a specific release version (default: latest)")
  .action(async (options: { version?: string }) => {
    const version = options.version || "latest";

    // Check Docker is running
    if (!checkDockerRunning()) {
      logError("Docker is not running. Please start Docker Desktop or Docker daemon.");
      process.exit(1);
    }

    // Check docker compose is available
    if (!checkDockerComposeAvailable()) {
      logError("Docker Compose is not available. Please install Docker Desktop or docker-compose.");
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

    // Download docker-compose.yml
    const spinner = p.spinner();
    spinner.start("Preparing Docker environment...");

    try {
      await downloadComposeFile(version);
      spinner.stop("Docker environment ready");
    } catch (error) {
      spinner.stop("Failed to prepare Docker environment");
      logError(`Error: ${error instanceof Error ? error.message : String(error)}`);
      process.exit(1);
    }

    // Generate shared API key for internal communication
    const sandboxApiKey = generateApiKey();

    // Build environment for docker compose
    const env: Record<string, string> = {
      ...process.env as Record<string, string>,
      ...configEnv,
      INCONVO_VERSION: version,
      INCONVO_SANDBOX_API_KEY: sandboxApiKey,
    };

    logGray("─".repeat(50));
    logInfo("Starting Inconvo...");
    logGray("  dev-server: http://localhost:26686");
    logGray("  sandbox:    http://localhost:8787");
    logGray("─".repeat(50));
    logGray("Press Ctrl+C to stop\n");

    // Run docker compose up
    const proc = spawn(
      "docker",
      ["compose", "-f", COMPOSE_FILE, "up", "--pull", "always", "--remove-orphans"],
      {
        env,
        stdio: "inherit",
      }
    );

    let shuttingDown = false;

    const shutdown = () => {
      if (shuttingDown) return;
      shuttingDown = true;

      console.log("\n\x1b[90mShutting down...\x1b[0m");

      // Run docker compose down
      try {
        execSync(`docker compose -f "${COMPOSE_FILE}" down`, {
          stdio: "inherit",
          env,
        });
      } catch {
        // Ignore errors during shutdown
      }

      process.exit(0);
    };

    process.on("SIGINT", shutdown);
    process.on("SIGTERM", shutdown);

    proc.on("error", (error) => {
      logError(`Failed to start Docker: ${error.message}`);
      process.exit(1);
    });

    proc.on("exit", (code) => {
      if (!shuttingDown) {
        process.exit(code || 0);
      }
    });
  });
