import * as fs from "fs/promises";
import * as path from "path";
import * as os from "os";
import { configSchema, type InconvoConfig } from "./schema.js";

const CONFIG_DIR = ".inconvo";
const CONFIG_FILE = "config.json";

export function getConfigDir(): string {
  return path.join(os.homedir(), CONFIG_DIR);
}

export function getConfigPath(): string {
  return path.join(getConfigDir(), CONFIG_FILE);
}

export async function ensureConfigDir(): Promise<void> {
  const configDir = getConfigDir();
  try {
    await fs.mkdir(configDir, { recursive: true });
  } catch (error) {
    if ((error as NodeJS.ErrnoException).code !== "EEXIST") {
      throw error;
    }
  }
}

export async function configExists(): Promise<boolean> {
  try {
    await fs.access(getConfigPath());
    return true;
  } catch {
    return false;
  }
}

export async function loadConfig(): Promise<InconvoConfig> {
  const configPath = getConfigPath();
  const content = await fs.readFile(configPath, "utf-8");
  const parsed = JSON.parse(content) as unknown;
  return configSchema.parse(parsed);
}

export async function saveConfig(config: InconvoConfig): Promise<void> {
  await ensureConfigDir();
  const configPath = getConfigPath();
  const content = JSON.stringify(config, null, 2);
  await fs.writeFile(configPath, content, "utf-8");
}

export function generateSecrets(): {
  internalApiKey: string;
  inconvoSecretKey: string;
} {
  return {
    internalApiKey: crypto.randomUUID(),
    inconvoSecretKey: `local-dev-${crypto.randomUUID()}`,
  };
}
