import { spawn, execSync, type ChildProcess } from "child_process";
import * as fs from "fs";
import * as path from "path";
import * as os from "os";
import type { ReleaseInfo } from "../release/downloader.js";

function getInconvoDir(): string {
  return path.join(os.homedir(), ".inconvo");
}

export interface RuntimeMode {
  type: "monorepo" | "release";
  devServerDir: string;
  sandboxDir: string;
}

/**
 * Detect whether we're running from monorepo source or a downloaded release
 */
export function detectRuntimeMode(release?: ReleaseInfo): RuntimeMode {
  // If a release is provided, use it
  if (release) {
    return {
      type: "release",
      devServerDir: release.devServerDir,
      sandboxDir: release.sandboxDir,
    };
  }

  // Check if we're in the monorepo
  const monorepoRoot = findMonorepoRoot();
  if (monorepoRoot) {
    return {
      type: "monorepo",
      devServerDir: path.join(monorepoRoot, "apps", "dev-server"),
      sandboxDir: path.join(monorepoRoot, "apps", "sandbox"),
    };
  }

  throw new Error(
    "Could not detect runtime mode. Run from monorepo or use --download flag."
  );
}

export function findMonorepoRoot(): string | null {
  let currentDir = process.cwd();

  while (currentDir !== path.parse(currentDir).root) {
    const workspaceFile = path.join(currentDir, "pnpm-workspace.yaml");
    if (fs.existsSync(workspaceFile)) {
      return currentDir;
    }
    currentDir = path.dirname(currentDir);
  }

  return null;
}

/**
 * Check if we're inside the monorepo
 */
export function isInMonorepo(): boolean {
  return findMonorepoRoot() !== null;
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
  const command =
    mode.type === "monorepo"
      ? "npx prisma db push --accept-data-loss"
      : "npx prisma db push --accept-data-loss";

  execSync(command, {
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

  if (mode.type === "monorepo") {
    return spawn("pnpm", ["next", "dev", "--port", "26686", "--turbopack"], {
      cwd: mode.devServerDir,
      env,
      stdio: ["inherit", "pipe", "pipe"],
      shell: true,
    });
  } else {
    // Release mode: run the standalone server.js
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

  if (mode.type === "monorepo") {
    return spawn("pnpm", ["wrangler", "dev"], {
      cwd: mode.sandboxDir,
      env,
      stdio: ["inherit", "pipe", "pipe"],
      shell: true,
    });
  } else {
    // Release mode: use npx wrangler
    return spawn("npx", ["wrangler", "dev"], {
      cwd: mode.sandboxDir,
      env,
      stdio: ["inherit", "pipe", "pipe"],
      shell: true,
    });
  }
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
