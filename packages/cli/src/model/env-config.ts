import * as dotenv from "dotenv";
import * as fs from "fs/promises";
import * as path from "path";
import { logWarning } from "../process/output.js";
import { pathExists } from "./fs-utils.js";

export async function findRepoRoot(cwd = process.cwd()): Promise<string> {
  let current = path.resolve(cwd);

  while (true) {
    if (await pathExists(path.join(current, ".git"))) {
      return current;
    }

    const parent = path.dirname(current);
    if (parent === current) {
      logWarning(
        "No .git directory found. Using current directory as repo root.",
      );
      return path.resolve(cwd);
    }
    current = parent;
  }
}

export interface RepoEnvConfig {
  apiKey?: string;
  apiBaseUrl?: string;
}

export async function loadRepoEnvConfig(
  repoRoot: string,
): Promise<RepoEnvConfig> {
  const envPath = path.join(repoRoot, ".env");
  let raw: string;
  try {
    raw = await fs.readFile(envPath, "utf8");
  } catch {
    return {};
  }

  const parsed = dotenv.parse(raw);
  return {
    apiKey: parsed.INCONVO_API_KEY?.trim() || undefined,
    apiBaseUrl: parsed.INCONVO_API_BASE_URL?.trim() || undefined,
  };
}
