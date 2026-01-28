import { spawn, execSync, type ChildProcess } from "child_process";
import * as fs from "fs";
import * as path from "path";

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

export function checkDockerRunning(): boolean {
  try {
    execSync("docker info", { stdio: "pipe" });
    return true;
  } catch {
    return false;
  }
}

export function initializePrismaDb(monorepoRoot: string): void {
  const devServerDir = path.join(monorepoRoot, "apps", "dev-server");
  execSync("npx prisma db push --accept-data-loss", {
    cwd: devServerDir,
    stdio: "pipe",
  });
}

const SANDBOX_BASE_URL = "http://localhost:8787";

export function generateSandboxApiKey(): string {
  return crypto.randomUUID();
}

export function spawnDevServer(
  monorepoRoot: string,
  sandboxApiKey: string
): ChildProcess {
  const devServerDir = path.join(monorepoRoot, "apps", "dev-server");

  return spawn("pnpm", ["next", "dev", "--port", "26686", "--turbopack"], {
    cwd: devServerDir,
    env: {
      ...process.env,
      INCONVO_SANDBOX_BASE_URL: SANDBOX_BASE_URL,
      INCONVO_SANDBOX_API_KEY: sandboxApiKey,
    },
    stdio: ["inherit", "pipe", "pipe"],
    shell: true,
  });
}

export function spawnSandbox(
  monorepoRoot: string,
  sandboxApiKey: string
): ChildProcess {
  const sandboxDir = path.join(monorepoRoot, "apps", "sandbox");

  return spawn("pnpm", ["wrangler", "dev"], {
    cwd: sandboxDir,
    env: {
      ...process.env,
      INTERNAL_API_KEY: sandboxApiKey,
      SKIP_BUCKET_MOUNT: "true",
      WRANGLER_SEND_METRICS: "false",
    },
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
