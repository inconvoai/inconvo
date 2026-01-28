import { Command } from "commander";
import * as p from "@clack/prompts";
import { envExists, runSetupWizard, readEnvFile } from "../wizard/setup.js";
import {
  detectRuntimeMode,
  isInMonorepo,
  checkDockerRunning,
  initializePrismaDb,
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
  .option("--version <version>", "Use a specific release version")
  .action(async (options: { version?: string }) => {
    let mode;

    // Determine runtime mode
    if (isInMonorepo()) {
      // Running from within the monorepo - use local source
      logInfo("Running in monorepo mode (using local source)");
      mode = detectRuntimeMode();
    } else {
      // Running outside monorepo - download release
      logInfo("Downloading Inconvo release...");
      try {
        const release = await ensureRelease(options.version);
        logInfo(`Using Inconvo v${release.version}`);
        mode = detectRuntimeMode(release);
      } catch (error) {
        logError(
          `Failed to download release: ${error instanceof Error ? error.message : String(error)}`
        );
        process.exit(1);
      }
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

    // Initialize Prisma database
    const spinner = p.spinner();
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

    // Handle process exits
    devServer.on("exit", (code: number | null) => {
      if (code !== 0 && code !== null) {
        logError(`dev-server exited with code ${code}`);
      }
    });

    sandbox.on("exit", (code: number | null) => {
      if (code !== 0 && code !== null) {
        logError(`sandbox exited with code ${code}`);
      }
    });

    // Keep the process running
    await new Promise(() => {});
  });
