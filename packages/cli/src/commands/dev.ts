import { Command } from "commander";
import * as p from "@clack/prompts";
import { configExists, loadConfig } from "../config/manager.js";
import { runSetupWizard } from "../wizard/setup.js";
import {
  findMonorepoRoot,
  buildDevServerEnv,
  buildSandboxEnv,
  checkDockerRunning,
  initializePrismaDb,
  spawnDevServer,
  spawnSandbox,
  setupShutdownHandler,
} from "../process/spawner.js";
import { createOutputHandler, logInfo, logError, logGray } from "../process/output.js";

export const devCommand = new Command("dev")
  .description("Start the Inconvo dev server and sandbox")
  .action(async () => {
    // Check for config, run wizard if missing
    if (!(await configExists())) {
      p.intro("Welcome to Inconvo! Let's set up your configuration first.");
      const config = await runSetupWizard();
      if (!config) {
        process.exit(1);
      }
    }

    const config = await loadConfig();

    // Find monorepo root
    const monorepoRoot = findMonorepoRoot();
    if (!monorepoRoot) {
      logError(
        "Could not find monorepo root. Make sure you are running this command from within the Inconvo repository."
      );
      process.exit(1);
    }

    logInfo(`Found monorepo at ${monorepoRoot}`);

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

    // Build environment variables
    const devServerEnv = buildDevServerEnv(config);
    const sandboxEnv = buildSandboxEnv(config);

    logGray("─".repeat(50));
    logInfo("Starting development environment...");
    logGray("  dev-server: http://localhost:26686");
    logGray("  sandbox:    http://localhost:8787");
    logGray("─".repeat(50));

    // Spawn processes
    const devServer = spawnDevServer(monorepoRoot, devServerEnv);
    const sandbox = spawnSandbox(monorepoRoot, sandboxEnv);

    // Setup output handlers
    devServer.stdout?.on("data", createOutputHandler("dev-server", "cyan"));
    devServer.stderr?.on("data", createOutputHandler("dev-server", "cyan"));
    sandbox.stdout?.on("data", createOutputHandler("sandbox", "yellow"));
    sandbox.stderr?.on("data", createOutputHandler("sandbox", "yellow"));

    // Setup shutdown handler
    setupShutdownHandler([devServer, sandbox]);

    // Handle process exits
    devServer.on("exit", (code) => {
      if (code !== 0 && code !== null) {
        logError(`dev-server exited with code ${code}`);
      }
    });

    sandbox.on("exit", (code) => {
      if (code !== 0 && code !== null) {
        logError(`sandbox exited with code ${code}`);
      }
    });

    // Keep the process running
    await new Promise(() => {});
  });
