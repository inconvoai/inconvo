import { findRepoRoot } from "../../model/env-config.js";
import { createApiClientFromOptions } from "../../model/cli-options.js";
import { normalizeAgentIds, validatePullFlags } from "../../model/selection.js";
import {
  parseModelPullCommandOptions,
  type ModelPullCommandOptions,
} from "./options.js";
import type { PlatformApiClient } from "../../model/api-client.js";

export interface ModelPullContext {
  parsedOptions: ModelPullCommandOptions;
  repoRoot: string;
  agentIds: string[];
  client: PlatformApiClient;
}

export async function resolveModelPullContext(
  options: unknown,
): Promise<ModelPullContext> {
  const parsedOptions = parseModelPullCommandOptions(options);
  const agentIds = normalizeAgentIds(parsedOptions.agentIds);

  const validationError = validatePullFlags({
    agentIds,
    allAgents: parsedOptions.allAgents,
    connectionId: parsedOptions.connectionId,
  });
  if (validationError) {
    throw new Error(validationError);
  }

  const repoRoot = await findRepoRoot();
  const client = await createApiClientFromOptions({
    apiBaseUrl: parsedOptions.apiBaseUrl,
    apiKey: parsedOptions.apiKey,
    repoRoot,
  });

  return {
    parsedOptions,
    repoRoot,
    agentIds,
    client,
  };
}
