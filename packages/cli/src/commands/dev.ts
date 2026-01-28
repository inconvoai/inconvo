import { Command } from "commander";
import * as p from "@clack/prompts";
import { envExists, runSetupWizard } from "../wizard/setup.js";
import {
  findMonorepoRoot,
  checkDockerRunning,
  initializePrismaDb,
  spawnDevServer,
  spawnSandbox,
  setupShutdownHandler,
  loadEnvFile,
} from "../process/spawner.js";
import {
  createOutputHandler,
  logInfo,
  logError,
  logGray,
} from "../process/output.js";

export const devCommand = new Command("dev")
  .description("Start the Inconvo dev server and sandbox")
  .action(async () => {
    // Find monorepo root first (needed for config check)
    const monorepoRoot = findMonorepoRoot();
    if (!monorepoRoot) {
      logError(
        "Could not find monorepo root. Make sure you are running this command from within the Inconvo repository."
      );
      process.exit(1);
    }

    logInfo(`Found monorepo at ${monorepoRoot}`);

    // Check for config, run wizard if missing
    if (!(await envExists(monorepoRoot))) {
      p.intro("Welcome to Inconvo! Let's set up your configuration first.");
      const success = await runSetupWizard(monorepoRoot);
      if (!success) {
        process.exit(1);
      }
    }

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
      initializePrismaDb(monorepoRoot);
      spinner.stop("Local database initialized");
    } catch (error) {
      spinner.stop("Failed to initialize local database");
      logError(
        `Error: ${error instanceof Error ? error.message : String(error)}`
      );
      process.exit(1);
    }

    // Load env file for sandbox API key
    const envVars = loadEnvFile(monorepoRoot);

    logGray("─".repeat(50));
    logInfo("Starting development environment...");
    logGray("  dev-server: http://localhost:26686");
    logGray("  sandbox:    http://localhost:8787");
    logGray("─".repeat(50));

    // Spawn processes
    const devServer = spawnDevServer(monorepoRoot);
    const sandbox = spawnSandbox(monorepoRoot, envVars);

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
