import { createApiClientFromOptions } from "./cli-options.js";
import { findRepoRoot } from "./config-store.js";
import {
  syncConnectionSnapshot,
  syncAgentUserContext,
  syncSingleAgentToWorkspace,
  type SyncScope,
  type ScopedSyncResult,
} from "./operations.js";
import type { ModelActionType } from "./types.js";
import type { PlatformApiClient } from "./api-client.js";
import { logWarning } from "../process/output.js";
import { optionRecord, parseString, parseBoolean } from "./parse-utils.js";

export type { SyncScope };

export interface MutationContext {
  client: PlatformApiClient;
  repoRoot: string;
  agentId: string;
  connectionId?: string;
  json: boolean;
  dryRun: boolean;
  noSync: boolean;
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
    noSync: parseBoolean(record.noSync),
  };
}

export interface SyncResult {
  scope: SyncScope | "full" | "none";
  skipped: boolean;
}

export async function runActionAndSync(params: {
  context: MutationContext;
  action: ModelActionType;
  payload: unknown;
  syncScope?: SyncScope;
}): Promise<{
  actionResult: unknown;
  sync: SyncResult;
}> {
  const actionResponse = await params.context.client.runModelAction(
    params.context.agentId,
    {
      action: params.action,
      payload: params.payload,
    },
  );

  if (params.context.noSync) {
    return {
      actionResult: actionResponse.result,
      sync: { scope: "none", skipped: true },
    };
  }

  let sync: SyncResult = { scope: "none", skipped: false };

  try {
    if (params.syncScope === "connection" && params.context.connectionId) {
      const result = await syncConnectionSnapshot({
        client: params.context.client,
        repoRoot: params.context.repoRoot,
        agentId: params.context.agentId,
        connectionId: params.context.connectionId,
      });
      sync = result;
    } else if (params.syncScope === "user-context") {
      const result = await syncAgentUserContext({
        client: params.context.client,
        repoRoot: params.context.repoRoot,
        agentId: params.context.agentId,
      });
      sync = result;
    } else {
      // Fallback to full sync
      await syncSingleAgentToWorkspace({
        client: params.context.client,
        repoRoot: params.context.repoRoot,
        agentId: params.context.agentId,
      });
      sync = { scope: "full", skipped: false };
    }
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
