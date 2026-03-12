import { Command } from "commander";
import * as p from "@clack/prompts";
import { COLORS, logInfo } from "../../process/output.js";
import type { ConnectionDetails } from "../../model/types.js";
import { runCliAction } from "../_shared/command-runtime.js";
import {
  resolveConnectionAgentContext,
  resolveConnectionTargetContext,
  addConnectionCommandOptions,
} from "../_shared/connection-command-factory.js";

function renderConnectionDetails(connection: ConnectionDetails): void {
  logInfo("Connection details:\n");
  console.log(
    `  ${COLORS.bold}${connection.name}${COLORS.reset}  ${COLORS.dim}${connection.id}${COLORS.reset}`,
  );
  console.log(`  status: ${connection.status ?? "UNKNOWN"}`);
  console.log(`  owner: ${connection.ownerAgentName}`);
  console.log(`  shared: ${connection.isShared ? "yes" : "no"}`);
  console.log(`  editable: ${connection.canEdit ? "yes" : "no"}`);
  if (connection.description) {
    console.log(`  description: ${connection.description}`);
  }
}

export const connectionGetCommand = addConnectionCommandOptions(
  new Command("get").description("Show metadata for a connection"),
).action((options) =>
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
        message: "Select a connection to inspect",
        options: connections.map((conn) => ({
          label: `${conn.name} (${conn.id})`,
          value: conn.id,
        })),
      });

      if (p.isCancel(selected)) {
        throw new Error("Selection cancelled.");
      }

      connectionId = selected;
    }

    const connection = await client.getConnection(agentId, connectionId);

    if (parsedOptions.json) {
      console.log(JSON.stringify({ connection }, null, 2));
      return;
    }

    renderConnectionDetails(connection);
  }),
);
