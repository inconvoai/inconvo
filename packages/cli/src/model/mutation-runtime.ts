import { createApiClientFromOptions } from "./cli-options.js";
import { findRepoRoot } from "./config-store.js";
import { syncSingleAgentToWorkspace } from "./operations.js";
import type { ModelActionType } from "./types.js";
import type { PlatformApiClient } from "./api-client.js";
import { logWarning } from "../process/output.js";

type OptionRecord = Record<string, unknown>;

function optionRecord(raw: unknown): OptionRecord {
  if (!raw || typeof raw !== "object") {
    return {};
  }
  return raw as OptionRecord;
}

function parseString(value: unknown): string | undefined {
  if (typeof value !== "string") {
    return undefined;
  }
  const trimmed = value.trim();
  return trimmed.length > 0 ? trimmed : undefined;
}

function parseBoolean(value: unknown): boolean {
  return value === true;
}

export interface MutationContext {
  client: PlatformApiClient;
  repoRoot: string;
  agentId: string;
  connectionId?: string;
  json: boolean;
  dryRun: boolean;
}

export async function resolveMutationContext(params: {
  options: unknown;
  requireConnection: boolean;
}): Promise<MutationContext> {
  const record = optionRecord(params.options);
  const agentId = parseString(record.agent);
  if (!agentId) {
    throw new Error("--agent is required.");
  }

  const connectionId = parseString(record.connection);
  if (params.requireConnection && !connectionId) {
    throw new Error("--connection is required.");
  }

  const repoRoot = await findRepoRoot();
  const client = await createApiClientFromOptions({
    apiBaseUrl: parseString(record.apiBaseUrl),
    apiKey: parseString(record.apiKey),
    repoRoot,
  });

  return {
    client,
    repoRoot,
    agentId,
    connectionId,
    json: parseBoolean(record.json),
    dryRun: parseBoolean(record.dryRun),
  };
}

export async function runActionAndSync(params: {
  context: MutationContext;
  action: ModelActionType;
  payload: unknown;
  syncConnectionId?: string;
}): Promise<{
  actionResult: unknown;
  sync: {
    pulledAgents: number;
    pulledConnections: number;
  };
}> {
  const actionResponse = await params.context.client.runModelAction(
    params.context.agentId,
    {
      action: params.action,
      payload: params.payload,
    },
  );

  let sync = { pulledAgents: 0, pulledConnections: 0 };
  try {
    sync = await syncSingleAgentToWorkspace({
      client: params.context.client,
      repoRoot: params.context.repoRoot,
      agentId: params.context.agentId,
      selectedConnectionId: params.syncConnectionId,
    });
  } catch (syncError) {
    logWarning(
      `Mutation succeeded but local sync failed: ${syncError instanceof Error ? syncError.message : String(syncError)}. Run 'inconvo model pull' to update local files.`,
    );
  }

  return {
    actionResult: actionResponse.result,
    sync,
  };
}
