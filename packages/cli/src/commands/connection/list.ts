import { Command } from "commander";
import { logInfo, COLORS } from "../../process/output.js";
import { runCliAction } from "../_shared/command-runtime.js";
import {
  resolveConnectionAgentContext,
  addConnectionCommandOptions,
} from "../_shared/connection-command-factory.js";

export const connectionListCommand = addConnectionCommandOptions(
  new Command("list").description("List connections for an agent"),
).action((options) =>
  runCliAction(async () => {
    const { parsedOptions, agentId, client } =
      await resolveConnectionAgentContext(options);

    const connections = await client.listAgentConnections(agentId);

    if (parsedOptions.json) {
      console.log(JSON.stringify({ connections }, null, 2));
      return;
    }

    if (connections.length === 0) {
      logInfo("No connections found for this agent.");
      return;
    }

    logInfo(`Found ${connections.length} connection(s):\n`);
    for (const conn of connections) {
      const sharedBadge = conn.isShared
        ? `${COLORS.magenta} [shared from ${conn.ownerAgentName}]${COLORS.reset}`
        : "";
      console.log(
        `  ${COLORS.bold}${conn.name}${COLORS.reset}  ${COLORS.dim}${conn.id}${COLORS.reset}  ${conn.status ?? ""}${sharedBadge}`,
      );
      if (conn.description) {
        console.log(`    ${COLORS.dim}${conn.description}${COLORS.reset}`);
      }
    }
  }),
);
