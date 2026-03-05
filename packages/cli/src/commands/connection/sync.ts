import { Command } from "commander";
import * as p from "@clack/prompts";
import { logInfo } from "../../process/output.js";
import { findRepoRoot } from "../../model/config-store.js";
import { syncSingleAgentToWorkspace } from "../../model/operations.js";
import { runCliAction } from "../_shared/command-runtime.js";
import {
  resolveConnectionAgentContext,
  resolveConnectionTargetContext,
} from "../_shared/connection-command-factory.js";

export const connectionSyncCommand = new Command("sync")
  .description("Trigger a full database resync for a connection")
  .requiredOption("--agent <agentId>", "Target agent id")
  .option("--connection <connectionId>", "Connection id to sync")
  .option("--json", "Print JSON output")
  .option("--api-key <apiKey>", "API key override (otherwise INCONVO_API_KEY)")
  .option(
    "--api-base-url <url>",
    "API base URL override (default: https://app.inconvo.ai)",
  )
  .action((options) =>
    runCliAction(async () => {
      const { parsedOptions, agentId, client } =
        await resolveConnectionAgentContext(options);

      let { connectionId } = resolveConnectionTargetContext({
        connectionId: parsedOptions.connectionId,
      });

      if (!connectionId) {
        const connections = await client.listAgentConnections(agentId);
        if (connections.length === 0) {
          throw new Error("No connections found for this agent.");
        }

        const selected = await p.select({
          message: "Select a connection to sync",
          options: connections.map((conn) => ({
            label: `${conn.name}  ${conn.id}`,
            value: conn.id,
          })),
        });

        if (p.isCancel(selected)) {
          throw new Error("Sync cancelled.");
        }

        connectionId = selected;
      }

      const { message } = await client.syncConnection(agentId, connectionId);
      const repoRoot = await findRepoRoot();
      const sync = await syncSingleAgentToWorkspace({
        client,
        repoRoot,
        agentId,
        selectedConnectionId: connectionId,
      });

      if (parsedOptions.json) {
        console.log(
          JSON.stringify({ message, connectionId, sync }, null, 2),
        );
        return;
      }

      logInfo(message);
    }),
  );
