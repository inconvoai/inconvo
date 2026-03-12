import { Command } from "commander";
import {
  resolveMutationContext,
  runActionAndSync,
  type MutationContext,
  type SyncScope,
  type SyncResult,
} from "../../../model/mutation-runtime.js";
import { DEFAULT_API_BASE_URL } from "../../../model/cli-options.js";
import type { ConnectionSemanticModelResponse, ModelActionType } from "../../../model/types.js";
import { logInfo } from "../../../process/output.js";

export type { MutationContext };
export { resolveMutationContext, runActionAndSync };

export function addCommonOptions(command: Command, requireConnection: boolean): Command {
  command.requiredOption("--agent <agentId>", "Target agent id");
  if (requireConnection) {
    command.requiredOption("--connection <connectionId>", "Target connection id");
  } else {
    command.option("--connection <connectionId>", "Optional connection id");
  }
  command.option("--api-key <apiKey>", "API key override (otherwise INCONVO_API_KEY)");
  command.option(
    "--api-base-url <url>",
    `API base URL override (default: ${DEFAULT_API_BASE_URL})`,
  );
  command.option("--dry-run", "Resolve and print action payload without applying");
  command.option("--json", "Print JSON output");
  command.option("--no-sync", "Skip post-mutation sync (use with 'inconvo model pull' after batch mutations)");
  return command;
}

export function printDryRunOutput(params: {
  context: MutationContext;
  action: string;
  payload: unknown;
  syncScope?: SyncScope;
}): void {
  const body = {
    dryRun: true,
    action: params.action,
    target: {
      agentId: params.context.agentId,
      connectionId: params.context.connectionId,
      syncScope: params.syncScope,
    },
    payload: params.payload,
  };

  if (params.context.json) {
    console.log(JSON.stringify(body, null, 2));
    return;
  }

  logInfo(`Dry run: ${params.action}`);
  logInfo(JSON.stringify(body, null, 2));
}

export async function loadSnapshot(
  context: MutationContext,
): Promise<ConnectionSemanticModelResponse> {
  if (!context.connectionId) {
    throw new Error("--connection is required.");
  }

  return context.client.getConnectionSemanticModel(
    context.agentId,
    context.connectionId,
  );
}

function formatSyncMessage(sync: SyncResult): string {
  if (sync.scope === "none") {
    return "Sync skipped (--no-sync).";
  }
  if (sync.skipped) {
    return "No changes detected (hash unchanged).";
  }
  return "Local snapshot updated.";
}

export function printMutationOutput(params: {
  context: MutationContext;
  action: string;
  result: unknown;
  sync: SyncResult;
}): void {
  if (params.context.json) {
    console.log(
      JSON.stringify(
        {
          action: params.action,
          result: params.result,
          sync: params.sync,
        },
        null,
        2,
      ),
    );
    return;
  }

  logInfo(`Action ${params.action} completed.`);
  logInfo(formatSyncMessage(params.sync));
}

export function printActionOnlyOutput(params: {
  context: MutationContext;
  action: string;
  result: unknown;
}): void {
  if (params.context.json) {
    console.log(
      JSON.stringify(
        {
          action: params.action,
          result: params.result,
        },
        null,
        2,
      ),
    );
    return;
  }

  logInfo(`Action ${params.action} completed.`);
  if (params.result !== undefined) {
    logInfo(JSON.stringify(params.result));
  }
}

export async function runMutation(params: {
  options: unknown;
  action: ModelActionType;
  requireConnection: boolean;
  buildPayload: (context: MutationContext) => Promise<unknown>;
  syncScope: SyncScope;
}): Promise<void> {
  const context = await resolveMutationContext({
    options: params.options,
    requireConnection: params.requireConnection,
  });
  const payload = await params.buildPayload(context);
  if (context.dryRun) {
    printDryRunOutput({
      context,
      action: params.action,
      payload,
      syncScope: params.syncScope,
    });
    return;
  }
  const response = await runActionAndSync({
    context,
    action: params.action,
    payload,
    syncScope: params.syncScope,
  });
  printMutationOutput({
    context,
    action: params.action,
    result: response.actionResult,
    sync: response.sync,
  });
}

export async function runActionOnly(params: {
  options: unknown;
  action: ModelActionType;
  requireConnection: boolean;
  buildPayload: (context: MutationContext) => Promise<unknown>;
}): Promise<void> {
  const context = await resolveMutationContext({
    options: params.options,
    requireConnection: params.requireConnection,
  });
  const payload = await params.buildPayload(context);
  if (context.dryRun) {
    printDryRunOutput({
      context,
      action: params.action,
      payload,
    });
    return;
  }
  const response = await context.client.runModelAction(context.agentId, {
    action: params.action,
    payload,
  });
  printActionOnlyOutput({
    context,
    action: params.action,
    result: response.result,
  });
}
