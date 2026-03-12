import type { Command } from "commander";
import {
  createApiClientFromOptions,
  isInteractiveEnvironment,
  DEFAULT_API_BASE_URL,
} from "../../model/cli-options.js";
import { findRepoRoot } from "../../model/config-store.js";
import type { PlatformApiClient } from "../../model/api-client.js";
import {
  parseConnectionCommandOptions,
  requireConnectionInNonInteractiveMode,
  type ConnectionCommandOptions,
} from "./options.js";

export function addConnectionCommandOptions(
  command: Command,
  options?: { requireConnection?: boolean },
): Command {
  command.requiredOption("--agent <agentId>", "Target agent id");
  if (options?.requireConnection) {
    command.requiredOption("--connection <connectionId>", "Target connection id");
  } else {
    command.option("--connection <connectionId>", "Connection id");
  }
  command.option("--json", "Print JSON output");
  command.option("--api-key <apiKey>", "API key override (otherwise INCONVO_API_KEY)");
  command.option(
    "--api-base-url <url>",
    `API base URL override (default: ${DEFAULT_API_BASE_URL})`,
  );
  return command;
}

export interface ConnectionAgentContext {
  parsedOptions: ConnectionCommandOptions;
  agentId: string;
  client: PlatformApiClient;
}

export interface ConnectionTargetContext {
  connectionId?: string;
}

export async function resolveConnectionAgentContext(
  options: unknown,
): Promise<ConnectionAgentContext> {
  const parsedOptions = parseConnectionCommandOptions(options);
  const agentId = parsedOptions.agentId;
  if (!agentId) {
    throw new Error("--agent is required.");
  }

  const repoRoot = await findRepoRoot();
  const client = await createApiClientFromOptions({
    apiBaseUrl: parsedOptions.apiBaseUrl,
    apiKey: parsedOptions.apiKey,
    repoRoot,
  });

  return {
    parsedOptions,
    agentId,
    client,
  };
}

export function resolveConnectionTargetContext(params: {
  connectionId?: string;
  interactive?: boolean;
}): ConnectionTargetContext {
  return {
    connectionId: requireConnectionInNonInteractiveMode({
      connectionId: params.connectionId,
      interactive: params.interactive ?? isInteractiveEnvironment(),
    }),
  };
}
