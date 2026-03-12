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

export const connectionLinkCommand = addConnectionCommandOptions(
  new Command("link").description("Link a shared connection from another agent"),
).action((options) =>
  runCliAction(async () => {
    const { parsedOptions, agentId, client } =
      await resolveConnectionAgentContext(options);

    let { connectionId } = resolveConnectionTargetContext({
      connectionId: parsedOptions.connectionId,
    });

    if (!connectionId) {
      const shareable = await client.listShareableConnections(agentId);
      if (shareable.length === 0) {
        throw new Error(
          "No shareable connections available in this organization.",
        );
      }

      const selected = await p.select({
        message: "Select a connection to link",
        options: shareable.map((conn) => ({
          label: `${conn.name} (${conn.databaseType ?? "UNKNOWN"}) — owner: ${conn.ownerAgent.name}`,
          value: conn.id,
        })),
      });

      if (p.isCancel(selected)) {
        throw new Error("Link cancelled.");
      }

      connectionId = selected;
    }

    await client.linkConnection(agentId, connectionId);
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
    logInfo(`Successfully linked connection ${connectionId}.`);
  }),
);
