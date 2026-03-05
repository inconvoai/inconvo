import { PlatformApiClient } from "./api-client.js";
import { findRepoRoot, loadLocalCliConfig } from "./config-store.js";

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
  const localConfig = await loadLocalCliConfig(repoRoot);

  const apiKey =
    options.apiKey ?? process.env.INCONVO_API_KEY ?? localConfig.apiKey;
  if (!apiKey) {
    throw new Error(
      "Missing API key. Pass --api-key, set INCONVO_API_KEY, or add apiKey to .inconvo/config.yaml.",
    );
  }

  const apiBaseUrl =
    options.apiBaseUrl ??
    process.env.INCONVO_API_BASE_URL ??
    localConfig.apiBaseUrl ??
    "https://app.inconvo.ai";

  return new PlatformApiClient({
    apiKey,
    baseUrl: apiBaseUrl,
  });
}

