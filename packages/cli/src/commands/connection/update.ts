import { Command } from "commander";
import * as p from "@clack/prompts";
import { findRepoRoot } from "../../model/env-config.js";
import { syncSingleAgentToWorkspace } from "../../model/operations.js";
import { logInfo, logWarning } from "../../process/output.js";
import { runCliAction } from "../_shared/command-runtime.js";
import {
  resolveConnectionAgentContext,
  resolveConnectionTargetContext,
  addConnectionCommandOptions,
} from "../_shared/connection-command-factory.js";

export const connectionUpdateCommand = addConnectionCommandOptions(
  new Command("update")
    .description("Update a connection description")
    .option("--description <text>", "New connection description")
    .option("--clear-description", "Clear the current connection description"),
).action((options) =>
  runCliAction(async () => {
    const { parsedOptions, agentId, client } =
      await resolveConnectionAgentContext(options);

    const { description, clearDescription } = parsedOptions;
    if (description !== undefined && clearDescription) {
      throw new Error(
        "--description and --clear-description cannot be used together.",
      );
    }
    if (description === undefined && !clearDescription) {
      throw new Error(
        "Provide either --description <text> or --clear-description.",
      );
    }
    const nextDescription = clearDescription ? null : description!;

    let { connectionId } = resolveConnectionTargetContext({
      connectionId: parsedOptions.connectionId,
    });

    if (!connectionId) {
      const editableConnections = (
        await client.listAgentConnections(agentId)
      ).filter((c) => !c.isShared);

      if (editableConnections.length === 0) {
        throw new Error("No editable connections found for this agent.");
      }

      const selected = await p.select({
        message: "Select a connection to update",
        options: editableConnections.map((conn) => ({
          label: `${conn.name} (${conn.id})`,
          value: conn.id,
        })),
      });

      if (p.isCancel(selected)) {
        throw new Error("Update cancelled.");
      }

      connectionId = selected;
    }

    const updated = await client.updateConnectionDescription(
      agentId,
      connectionId,
      nextDescription,
    );

    let sync:
      | { pulledAgents: number; pulledConnections: number }
      | undefined;
    try {
      const repoRoot = await findRepoRoot();
      sync = await syncSingleAgentToWorkspace({
        client,
        repoRoot,
        agentId,
        selectedConnectionId: connectionId,
      });
    } catch (syncError) {
      if (!parsedOptions.json) {
        logWarning(
          `Description updated but local snapshot sync failed: ${syncError instanceof Error ? syncError.message : String(syncError)}. Run 'inconvo model pull' to update local files.`,
        );
      }
    }

    if (parsedOptions.json) {
      console.log(
        JSON.stringify(
          { connection: updated, ...(sync ? { sync } : {}) },
          null,
          2,
        ),
      );
      return;
    }

    logInfo(
      nextDescription === null
        ? `Cleared description for ${updated.name}.`
        : `Updated description for ${updated.name}.`,
    );
  }),
);
