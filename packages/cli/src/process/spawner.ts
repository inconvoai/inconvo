import { spawn, execSync, type ChildProcess } from "child_process";
import * as path from "path";
import * as os from "os";
import type { ReleaseInfo } from "../release/downloader.js";

export interface RuntimeMode {
  type: "release";
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

export function initializePrismaDb(mode: RuntimeMode): void {
  // Use bundled prisma directly to avoid npm/pnpm conflicts
  const prismaIndex = path.join(
    mode.devServerDir,
    "node_modules",
    "prisma",
    "build",
    "index.js"
  );
  execSync(`node "${prismaIndex}" db push --accept-data-loss`, {
    cwd: mode.devServerDir,
    stdio: "pipe",
    shell: true,
    env: {
      ...getCleanEnv(),
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

export function spawnSandbox(
  mode: RuntimeMode,
  sandboxApiKey: string
): ChildProcess {
  const env = {
    ...getCleanEnv(),
    INTERNAL_API_KEY: sandboxApiKey,
    SKIP_BUCKET_MOUNT: "true",
    WRANGLER_SEND_METRICS: "false",
  };

  // Use wrangler binary directly to avoid npm/pnpm conflicts
  const wranglerBin = path.join(mode.sandboxDir, "node_modules", ".bin", "wrangler");
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
