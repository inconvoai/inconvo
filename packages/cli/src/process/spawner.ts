import { spawn, execSync, type ChildProcess } from "child_process";
import type { ReleaseInfo } from "../release/downloader.js";

export interface RuntimeMode {
  type: "release";
  devServerDir: string;
  sandboxDir: string;
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
  execSync("npx prisma db push --accept-data-loss", {
    cwd: mode.devServerDir,
    stdio: "pipe",
    env: {
      ...process.env,
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
    ...process.env,
    ...configEnv,
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
    ...process.env,
    INTERNAL_API_KEY: sandboxApiKey,
    SKIP_BUCKET_MOUNT: "true",
    WRANGLER_SEND_METRICS: "false",
  };

  // Use npx wrangler from the release bundle
  return spawn("npx", ["wrangler", "dev"], {
    cwd: mode.sandboxDir,
    env,
    stdio: ["inherit", "pipe", "pipe"],
    shell: true,
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
