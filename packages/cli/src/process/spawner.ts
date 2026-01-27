import { spawn, execSync, type ChildProcess } from "child_process";
import * as fs from "fs";
import * as path from "path";
import type { InconvoConfig } from "../config/schema.js";

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

export function buildDevServerEnv(
  config: InconvoConfig
): Record<string, string> {
  return {
    ...process.env,
    DATABASE_DIALECT: config.database.dialect,
    INCONVO_DATABASE_URL: config.database.url,
    ...(config.database.schema && {
      INCONVO_DATABASE_SCHEMA: config.database.schema,
    }),
    OPENAI_API_KEY: config.apiKeys.openai,
    LANGCHAIN_API_KEY: config.apiKeys.langchain,
    INCONVO_SECRET_KEY: config.secrets.inconvoSecretKey,
    INCONVO_SANDBOX_BASE_URL: "http://localhost:8787",
    INCONVO_SANDBOX_API_KEY: config.secrets.internalApiKey,
  } as Record<string, string>;
}

export function buildSandboxEnv(config: InconvoConfig): Record<string, string> {
  return {
    ...process.env,
    INTERNAL_API_KEY: config.secrets.internalApiKey,
    SKIP_BUCKET_MOUNT: "true",
    WRANGLER_SEND_METRICS: "false",
  } as Record<string, string>;
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
  execSync("npx prisma db push --skip-generate", {
    cwd: devServerDir,
    stdio: "pipe",
  });
}

export interface SpawnedProcess {
  name: string;
  process: ChildProcess;
}

export function spawnDevServer(
  monorepoRoot: string,
  env: Record<string, string>
): ChildProcess {
  const devServerDir = path.join(monorepoRoot, "apps", "dev-server");

  return spawn("pnpm", ["next", "dev", "--port", "26686", "--turbopack"], {
    cwd: devServerDir,
    env,
    stdio: ["inherit", "pipe", "pipe"],
    shell: true,
  });
}

export function spawnSandbox(
  monorepoRoot: string,
  env: Record<string, string>
): ChildProcess {
  const sandboxDir = path.join(monorepoRoot, "apps", "sandbox");

  return spawn("pnpm", ["wrangler", "dev"], {
    cwd: sandboxDir,
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
