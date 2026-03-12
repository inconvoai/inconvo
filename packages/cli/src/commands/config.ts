import { Command } from "commander";
import * as p from "@clack/prompts";
import { logInfo, logBold, logDim } from "../process/output.js";
import { runCliAction } from "./_shared/command-runtime.js";
import {
  findRepoRoot,
  loadLocalCliConfig,
  writeLocalCliConfig,
  LOCAL_CONFIG_PATH,
} from "../model/config-store.js";
import { DEFAULT_API_BASE_URL } from "../model/cli-options.js";

function maskApiKey(key: string): string {
  if (key.length <= 8) return "***";
  return `${key.slice(0, 4)}${"*".repeat(key.length - 8)}${key.slice(-4)}`;
}

const configSetCommand = new Command("set")
  .description("Set local API credentials in .inconvo/config.yaml")
  .option("--api-key <apiKey>", "API key to store")
  .option(
    "--api-base-url <url>",
    `API base URL to store (default: ${DEFAULT_API_BASE_URL})`,
  )
  .action((options: { apiKey?: string; apiBaseUrl?: string }) =>
    runCliAction(async () => {
      const repoRoot = await findRepoRoot();
      const existing = await loadLocalCliConfig(repoRoot);

      let apiKey = options.apiKey?.trim();
      let apiBaseUrl = options.apiBaseUrl?.trim();

      if (!apiKey && !apiBaseUrl) {
        // Interactive mode
        const keyInput = await p.text({
          message: "API key",
          placeholder: existing.apiKey ? maskApiKey(existing.apiKey) : "sk-...",
          validate: (value) => {
            if (!value?.trim() && !existing.apiKey) {
              return "API key is required.";
            }
          },
        });
        if (p.isCancel(keyInput)) throw new Error("Cancelled.");
        if (keyInput.trim()) apiKey = keyInput.trim();

        const urlInput = await p.text({
          message: "API base URL",
          placeholder: existing.apiBaseUrl ?? DEFAULT_API_BASE_URL,
          initialValue: existing.apiBaseUrl ?? DEFAULT_API_BASE_URL,
        });
        if (p.isCancel(urlInput)) throw new Error("Cancelled.");
        if (urlInput.trim()) apiBaseUrl = urlInput.trim();
      }

      const updated = {
        apiKey: apiKey ?? existing.apiKey,
        apiBaseUrl:
          apiBaseUrl === undefined
            ? existing.apiBaseUrl
            : apiBaseUrl === DEFAULT_API_BASE_URL
              ? undefined
              : apiBaseUrl,
      };

      await writeLocalCliConfig(repoRoot, updated);
      logInfo(`Config written to ${LOCAL_CONFIG_PATH}`);
    }),
  );

const configViewCommand = new Command("view")
  .description("Show the current local config from .inconvo/config.yaml")
  .action(() =>
    runCliAction(async () => {
      const repoRoot = await findRepoRoot();
      const config = await loadLocalCliConfig(repoRoot);

      if (!config.apiKey && !config.apiBaseUrl) {
        logInfo(
          `No local config found at ${LOCAL_CONFIG_PATH}. Run 'inconvo config set' to create one.`,
        );
        return;
      }

      console.log("");
      logBold(`  ${LOCAL_CONFIG_PATH}`);
      console.log("");
      if (config.apiKey) {
        logDim(`  apiKey     ${maskApiKey(config.apiKey)}`);
      }
      if (config.apiBaseUrl) {
        logDim(`  apiBaseUrl ${config.apiBaseUrl}`);
      }
      console.log("");
    }),
  );

export const configCommand = new Command("config")
  .description("Manage local CLI configuration (.inconvo/config.yaml)")
  .addCommand(configSetCommand)
  .addCommand(configViewCommand);
