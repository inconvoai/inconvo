import { PlatformApiClient } from "./api-client.js";
import {
  findRepoRoot,
  loadRepoEnvConfig,
} from "./env-config.js";

export const DEFAULT_API_BASE_URL = "https://app.inconvo.ai";

export function isInteractiveEnvironment(): boolean {
  const isCi = process.env.CI === "true" || process.env.CI === "1";
  return Boolean(process.stdin.isTTY && process.stdout.isTTY && !isCi);
}

export async function createApiClientFromOptions(options: {
  apiBaseUrl?: string;
  apiKey?: string;
  repoRoot?: string;
}): Promise<PlatformApiClient> {
  const repoRoot = options.repoRoot ?? (await findRepoRoot());
  const repoEnvConfig = await loadRepoEnvConfig(repoRoot);

  const apiKey =
    options.apiKey ??
    process.env.INCONVO_API_KEY ??
    repoEnvConfig.apiKey;
  if (!apiKey) {
    throw new Error(
      "Missing API key. Pass --api-key, set INCONVO_API_KEY, or add it to the repo .env.",
    );
  }

  const apiBaseUrl =
    options.apiBaseUrl ??
    process.env.INCONVO_API_BASE_URL ??
    repoEnvConfig.apiBaseUrl ??
    DEFAULT_API_BASE_URL;

  return new PlatformApiClient({
    apiKey,
    baseUrl: apiBaseUrl,
  });
}
