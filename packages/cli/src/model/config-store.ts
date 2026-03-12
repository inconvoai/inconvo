import * as fs from "fs/promises";
import * as path from "path";
import YAML from "yaml";
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

export interface LocalCliConfig {
  apiKey?: string;
  apiBaseUrl?: string;
}

export const LOCAL_CONFIG_PATH = path.join(".inconvo", "config.yaml");

export async function loadLocalCliConfig(
  repoRoot: string,
): Promise<LocalCliConfig> {
  const configPath = path.join(repoRoot, LOCAL_CONFIG_PATH);
  let raw: string;
  try {
    raw = await fs.readFile(configPath, "utf8");
  } catch {
    return {};
  }

  try {
    const parsed = YAML.parse(raw);
    if (!parsed || typeof parsed !== "object") {
      return {};
    }
    const record = parsed as Record<string, unknown>;
    return {
      apiKey: typeof record.apiKey === "string" ? record.apiKey.trim() || undefined : undefined,
      apiBaseUrl: typeof record.apiBaseUrl === "string" ? record.apiBaseUrl.trim() || undefined : undefined,
    };
  } catch {
    return {};
  }
}

export async function writeLocalCliConfig(
  repoRoot: string,
  config: LocalCliConfig,
): Promise<void> {
  const configPath = path.join(repoRoot, LOCAL_CONFIG_PATH);
  await fs.mkdir(path.dirname(configPath), { recursive: true });
  const record: Record<string, string> = {};
  if (config.apiKey) record.apiKey = config.apiKey;
  if (config.apiBaseUrl) record.apiBaseUrl = config.apiBaseUrl;
  const header =
    "# Inconvo local config — DO NOT COMMIT (this file is gitignored)\n";
  await fs.writeFile(configPath, header + YAML.stringify(record), { encoding: "utf8", mode: 0o600 });
}
