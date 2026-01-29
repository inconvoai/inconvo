import { spawn, execSync, type ChildProcess } from "child_process";
import * as fs from "fs";
import * as path from "path";
import * as os from "os";
import type { ReleaseInfo } from "../release/downloader.js";

export interface RuntimeMode {
  type: "release";
  releaseDir: string;
  devServerDir: string;
  sandboxDir: string;
}

/**
 * Get the path to the local SQLite database (persists across CLI versions)
 */
function getLocalDbPath(): string {
  return path.join(os.homedir(), ".inconvo", "inconvo.db");
}

/**
 * Get a clean environment without pnpm-specific variables that confuse npm
 */
function getCleanEnv(): Record<string, string> {
  const env: Record<string, string> = {};
  const skipPrefixes = ["npm_", "PNPM_", "NVM_"];
  const skipKeys = new Set([
    "npm_config_globalconfig",
    "npm_config_userconfig",
    "npm_execpath",
    "npm_node_execpath",
  ]);

  for (const [key, value] of Object.entries(process.env)) {
    if (value === undefined) continue;
    if (skipKeys.has(key.toLowerCase())) continue;
    if (skipPrefixes.some((prefix) => key.startsWith(prefix))) continue;
    env[key] = value;
  }

  return env;
}

/**
 * Create runtime mode from a downloaded release
 */
export function createRuntimeMode(release: ReleaseInfo): RuntimeMode {
  return {
    type: "release",
    releaseDir: release.releaseDir,
    devServerDir: release.devServerDir,
    sandboxDir: release.sandboxDir,
  };
}

export function checkDockerRunning(): boolean {
  try {
    execSync("docker info", { stdio: "pipe" });
    return true;
  } catch {
    return false;
  }
}

// Pinned versions for runtime dependencies
const PRISMA_VERSION = "7.3.0";
const WRANGLER_VERSION = "4.14.1";

/**
 * Get path to CLI tools directory (separate from bundle node_modules)
 */
function getToolsDir(): string {
  return path.join(os.homedir(), ".inconvo", "tools");
}

export function ensureDevServerDeps(mode: RuntimeMode): void {
  const toolsDir = getToolsDir();
  const prismaDir = path.join(toolsDir, "node_modules", "prisma");

  // Check if prisma exists, install if not
  try {
    fs.accessSync(prismaDir);
  } catch {
    fs.mkdirSync(toolsDir, { recursive: true });
    execSync(`npm install prisma@${PRISMA_VERSION} dotenv@17.2.3 --no-save`, {
      cwd: toolsDir,
      stdio: "pipe",
      env: getCleanEnv(),
    });
  }
}

export function initializePrismaDb(mode: RuntimeMode): void {
  const toolsDir = getToolsDir();
  const toolsNodeModules = path.join(toolsDir, "node_modules");
  const prismaIndex = path.join(toolsNodeModules, "prisma", "build", "index.js");

  execSync(`node "${prismaIndex}" db push --accept-data-loss`, {
    cwd: mode.devServerDir,
    stdio: "pipe",
    env: {
      ...getCleanEnv(),
      // Include tools node_modules so prisma config can find dotenv
      NODE_PATH: toolsNodeModules,
      INCONVO_LOCAL_DB_PATH: getLocalDbPath(),
      SKIP_ENV_VALIDATION: "true",
    },
  });
}

export function generateSandboxApiKey(): string {
  return crypto.randomUUID();
}

const SANDBOX_BASE_URL = "http://localhost:8787";

export function spawnDevServer(
  mode: RuntimeMode,
  sandboxApiKey: string,
  configEnv: Record<string, string>
): ChildProcess {
  const env = {
    ...getCleanEnv(),
    ...configEnv,
    INCONVO_LOCAL_DB_PATH: getLocalDbPath(),
    INCONVO_SANDBOX_BASE_URL: SANDBOX_BASE_URL,
    INCONVO_SANDBOX_API_KEY: sandboxApiKey,
    SKIP_ENV_VALIDATION: "true",
  };

  // Run the standalone server.js from the release bundle
  return spawn("node", ["server.js"], {
    cwd: mode.devServerDir,
    env: {
      ...env,
      PORT: "26686",
      HOSTNAME: "localhost",
    },
    stdio: ["inherit", "pipe", "pipe"],
  });
}

export function ensureSandboxDeps(): void {
  const toolsDir = getToolsDir();
  const wranglerDir = path.join(toolsDir, "node_modules", "wrangler");

  // Check if wrangler exists, install if not
  try {
    fs.accessSync(wranglerDir);
  } catch {
    // Install wrangler in tools directory for the user's platform
    fs.mkdirSync(toolsDir, { recursive: true });
    execSync(`npm install wrangler@${WRANGLER_VERSION} --no-save`, {
      cwd: toolsDir,
      stdio: "pipe",
      env: getCleanEnv(),
    });
  }
}

export function spawnSandbox(
  mode: RuntimeMode,
  sandboxApiKey: string
): ChildProcess {
  const toolsDir = getToolsDir();
  const wranglerBin = path.join(toolsDir, "node_modules", ".bin", "wrangler");

  const env = {
    ...getCleanEnv(),
    INTERNAL_API_KEY: sandboxApiKey,
    SKIP_BUCKET_MOUNT: "true",
    WRANGLER_SEND_METRICS: "false",
  };

  return spawn(wranglerBin, ["dev"], {
    cwd: mode.sandboxDir,
    env,
    stdio: ["inherit", "pipe", "pipe"],
  });
}

export function setupShutdownHandler(processes: ChildProcess[]): void {
  const shutdown = () => {
    console.log("\n\x1b[90mShutting down...\x1b[0m");
    for (const proc of processes) {
      if (!proc.killed) {
        proc.kill("SIGTERM");
      }
    }
  };

  process.on("SIGINT", shutdown);
  process.on("SIGTERM", shutdown);

  process.on("exit", () => {
    for (const proc of processes) {
      if (!proc.killed) {
        proc.kill("SIGKILL");
      }
    }
  });
}
