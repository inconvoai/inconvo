import { Command } from "commander";
import * as p from "@clack/prompts";
import { logInfo } from "../../process/output.js";
import { findRepoRoot } from "../../model/config-store.js";
import { syncSingleAgentToWorkspace } from "../../model/operations.js";
import { runCliAction } from "../_shared/command-runtime.js";
import {
  resolveConnectionAgentContext,
  resolveConnectionTargetContext,
  addConnectionCommandOptions,
} from "../_shared/connection-command-factory.js";

export const connectionUnlinkCommand = addConnectionCommandOptions(
  new Command("unlink").description("Remove a linked shared connection"),
).action((options) =>
  runCliAction(async () => {
    const { parsedOptions, agentId, client } =
      await resolveConnectionAgentContext(options);

    let { connectionId } = resolveConnectionTargetContext({
      connectionId: parsedOptions.connectionId,
    });

    if (!connectionId) {
      const connections = await client.listAgentConnections(agentId);
      const sharedConnections = connections.filter((c) => c.isShared);

      if (sharedConnections.length === 0) {
        throw new Error(
          "No shared connections to unlink for this agent.",
        );
      }

      const selected = await p.select({
        message: "Select a shared connection to unlink",
        options: sharedConnections.map((conn) => ({
          label: `${conn.name} (from ${conn.ownerAgentName})`,
          value: conn.id,
        })),
      });

      if (p.isCancel(selected)) {
        throw new Error("Unlink cancelled.");
      }

      connectionId = selected;
    }

    await client.unlinkConnection(agentId, connectionId);
    const repoRoot = await findRepoRoot();
    const sync = await syncSingleAgentToWorkspace({
      client,
      repoRoot,
      agentId,
    });
    if (parsedOptions.json) {
      console.log(
        JSON.stringify(
          {
            success: true,
            connectionId,
            sync,
          },
          null,
          2,
        ),
      );
      return;
    }
    logInfo(`Successfully unlinked connection ${connectionId}.`);
  }),
);
