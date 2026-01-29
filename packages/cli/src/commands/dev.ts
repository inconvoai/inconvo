import { Command } from "commander";
import { execSync, spawn } from "child_process";
import * as fs from "fs";
import * as path from "path";
import * as os from "os";
import { fileURLToPath } from "url";
import * as p from "@clack/prompts";
import { envExists, runSetupWizard, readEnvFile } from "../wizard/setup.js";
import { logInfo, logError, logGray } from "../process/output.js";

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const INCONVO_DIR = path.join(os.homedir(), ".inconvo");
const COMPOSE_FILE = path.join(INCONVO_DIR, "docker-compose.yml");
// Bundled compose file (shipped with npm package)
const BUNDLED_COMPOSE_FILE = path.join(__dirname, "..", "..", "docker-compose.yml");

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
  const cmd = platform === "darwin" ? "open" : platform === "win32" ? "start" : "xdg-open";
  try {
    execSync(`${cmd} ${url}`, { stdio: "ignore" });
  } catch {
    // Silently fail if browser can't be opened
  }
}

/**
 * Wait for a URL to become available
 */
async function waitForServer(url: string, maxAttempts = 30): Promise<boolean> {
  for (let i = 0; i < maxAttempts; i++) {
    try {
      const response = await fetch(url);
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

function copyBundledComposeFile(): void {
  // Copy the bundled compose file to ~/.inconvo/
  // This ensures the compose file version matches the CLI version
  fs.mkdirSync(INCONVO_DIR, { recursive: true });
  fs.copyFileSync(BUNDLED_COMPOSE_FILE, COMPOSE_FILE);
}

export const devCommand = new Command("dev")
  .description("Start the Inconvo dev server and sandbox")
  .option("--image-version <version>", "Use a specific Docker image version (default: latest)")
  .action(async (options: { imageVersion?: string }) => {
    const imageVersion = options.imageVersion || "latest";

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

    // Copy bundled docker-compose.yml to ~/.inconvo/
    const spinner = p.spinner();
    spinner.start("Preparing Docker environment...");

    try {
      copyBundledComposeFile();
      spinner.stop("Docker environment ready");
    } catch (error) {
      spinner.stop("Failed to prepare Docker environment");
      logError(`Error: ${error instanceof Error ? error.message : String(error)}`);
      process.exit(1);
    }

    // Generate shared API key for internal communication
    const sandboxApiKey = generateApiKey();

    // Rewrite localhost to host.docker.internal for Docker access to host services
    const dockerEnv = { ...configEnv };
    if (dockerEnv.INCONVO_DATABASE_URL) {
      dockerEnv.INCONVO_DATABASE_URL = rewriteLocalhostForDocker(dockerEnv.INCONVO_DATABASE_URL);
    }

    // Build environment for docker compose
    const env: Record<string, string> = {
      ...process.env as Record<string, string>,
      ...dockerEnv,
      INCONVO_VERSION: imageVersion,
      INCONVO_SANDBOX_API_KEY: sandboxApiKey,
    };

    const devServerUrl = "http://localhost:26686";

    logGray("─".repeat(50));
    logInfo("Starting Inconvo...");
    logGray(`  dev-server: ${devServerUrl}`);
    logGray("  sandbox:    http://localhost:8787");
    logGray("─".repeat(50));
    logGray("Press Ctrl+C to stop\n");

    // Wait for server to be ready, then open browser
    (async () => {
      const ready = await waitForServer(devServerUrl);
      if (ready) {
        openBrowser(devServerUrl);
      }
    })();

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
