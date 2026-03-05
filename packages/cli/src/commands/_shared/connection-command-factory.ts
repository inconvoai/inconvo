import {
  createApiClientFromOptions,
  isInteractiveEnvironment,
} from "../../model/cli-options.js";
import { findRepoRoot } from "../../model/config-store.js";
import { PlatformApiClient } from "../../model/api-client.js";
import {
  parseConnectionCommandOptions,
  requireConnectionInNonInteractiveMode,
  type ConnectionCommandOptions,
} from "./options.js";

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
