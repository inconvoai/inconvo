import { Command } from "commander";
import { logInfo, COLORS } from "../../process/output.js";
import { runCliAction } from "../_shared/command-runtime.js";
import { resolveConnectionAgentContext } from "../_shared/connection-command-factory.js";

export const connectionListCommand = new Command("list")
  .description("List connections for an agent")
  .requiredOption("--agent <agentId>", "Target agent id")
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
      }
    }),
  );
