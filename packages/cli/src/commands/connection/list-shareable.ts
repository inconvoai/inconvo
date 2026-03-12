import { Command } from "commander";
import { logInfo, COLORS } from "../../process/output.js";
import { runCliAction } from "../_shared/command-runtime.js";
import {
  resolveConnectionAgentContext,
  addConnectionCommandOptions,
} from "../_shared/connection-command-factory.js";

export const connectionListShareableCommand = addConnectionCommandOptions(
  new Command("list-shareable").description(
    "List connections from other agents available for linking",
  ),
).action((options) =>
  runCliAction(async () => {
    const { parsedOptions, agentId, client } =
      await resolveConnectionAgentContext(options);

    const connections = await client.listShareableConnections(agentId);

    if (parsedOptions.json) {
      console.log(JSON.stringify({ connections }, null, 2));
      return;
    }

    if (connections.length === 0) {
      logInfo("No shareable connections found in this organization.");
      return;
    }

    logInfo(`Found ${connections.length} shareable connection(s):\n`);
    for (const conn of connections) {
      console.log(
        `  ${COLORS.bold}${conn.name}${COLORS.reset}  ${COLORS.dim}${conn.id}${COLORS.reset}  ${conn.databaseType ?? "UNKNOWN"}  ${COLORS.cyan}owner: ${conn.ownerAgent.name}${COLORS.reset}`,
      );
      if (conn.description) {
        console.log(`    ${COLORS.dim}${conn.description}${COLORS.reset}`);
      }
    }
  }),
);
