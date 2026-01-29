import { Command } from "commander";
import * as p from "@clack/prompts";
import { envExists, runSetupWizard, readEnvFile } from "../wizard/setup.js";
import {
  createRuntimeMode,
  checkDockerRunning,
  ensureDevServerDeps,
  initializePrismaDb,
  ensureSandboxDeps,
  spawnDevServer,
  spawnSandbox,
  setupShutdownHandler,
  generateSandboxApiKey,
} from "../process/spawner.js";
import { ensureRelease } from "../release/downloader.js";
import {
  createOutputHandler,
  logInfo,
  logError,
  logGray,
} from "../process/output.js";

export const devCommand = new Command("dev")
  .description("Start the Inconvo dev server and sandbox")
  .option("--release <version>", "Use a specific release version (default: latest)")
  .action(async (options: { release?: string }) => {
    // Download the release (uses latest if no version specified)
    logInfo("Checking for Inconvo release...");
    let mode;
    try {
      const release = await ensureRelease(options.release);
      logInfo(`Using Inconvo v${release.version}`);
      mode = createRuntimeMode(release);
    } catch (error) {
      logError(
        `Failed to download release: ${error instanceof Error ? error.message : String(error)}`
      );
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

    // Check Docker is running (required for sandbox containers)
    if (!checkDockerRunning()) {
      logError(
        "Docker is not running. Please start Docker Desktop or Colima to run the sandbox."
      );
      process.exit(1);
    }

    logInfo("Docker is running");

    // Ensure dev-server dependencies (prisma)
    const spinner = p.spinner();
    spinner.start("Installing dependencies...");

    try {
      ensureDevServerDeps(mode);
      spinner.stop("Dependencies installed");
    } catch (error) {
      spinner.stop("Failed to install dependencies");
      logError(
        `Error: ${error instanceof Error ? error.message : String(error)}`
      );
      process.exit(1);
    }

    // Initialize Prisma database
    spinner.start("Initializing local database...");

    try {
      initializePrismaDb(mode);
      spinner.stop("Local database initialized");
    } catch (error) {
      spinner.stop("Failed to initialize local database");
      logError(
        `Error: ${error instanceof Error ? error.message : String(error)}`
      );
      process.exit(1);
    }

    // Ensure sandbox dependencies are installed (platform-specific)
    spinner.start("Checking sandbox dependencies...");
    try {
      ensureSandboxDeps();
      spinner.stop("Sandbox dependencies ready");
    } catch (error) {
      spinner.stop("Failed to install sandbox dependencies");
      logError(
        `Error: ${error instanceof Error ? error.message : String(error)}`
      );
      process.exit(1);
    }

    // Generate shared API key for sandbox communication
    const sandboxApiKey = generateSandboxApiKey();

    logGray("─".repeat(50));
    logInfo("Starting development environment...");
    logGray("  dev-server: http://localhost:26686");
    logGray("  sandbox:    http://localhost:8787");
    logGray("─".repeat(50));

    // Spawn processes with shared API key
    const devServer = spawnDevServer(mode, sandboxApiKey, configEnv);
    const sandbox = spawnSandbox(mode, sandboxApiKey);

    // Setup output handlers
    devServer.stdout?.on("data", createOutputHandler("dev-server", "cyan"));
    devServer.stderr?.on("data", createOutputHandler("dev-server", "cyan"));
    sandbox.stdout?.on("data", createOutputHandler("sandbox", "yellow"));
    sandbox.stderr?.on("data", createOutputHandler("sandbox", "yellow"));

    // Setup shutdown handler
    setupShutdownHandler([devServer, sandbox]);

    // Wait for either process to exit
    await new Promise<void>((resolve) => {
      let exited = 0;
      const onExit = (name: string) => (code: number | null) => {
        if (code !== 0 && code !== null) {
          logError(`${name} exited with code ${code}`);
        }
        exited++;
        if (exited === 2) {
          resolve();
        }
      };

      devServer.on("exit", onExit("dev-server"));
      sandbox.on("exit", onExit("sandbox"));
    });
  });
