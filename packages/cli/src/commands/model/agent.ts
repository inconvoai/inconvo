import { Command } from "commander";
import { createApiClientFromOptions } from "../../model/cli-options.js";
import { COLORS, logInfo } from "../../process/output.js";
import { runCliAction } from "../_shared/command-runtime.js";

const modelAgentListCommand = new Command("list")
  .description("List agents available in the current organization")
  .option("--json", "Print JSON output")
  .option("--api-key <apiKey>", "API key override (otherwise INCONVO_API_KEY)")
  .option(
    "--api-base-url <url>",
    "API base URL override (default: https://app.inconvo.ai)",
  )
  .action((options: {
    apiKey?: string;
    apiBaseUrl?: string;
    json?: boolean;
  }) =>
    runCliAction(async () => {
      const client = await createApiClientFromOptions({
        apiKey: options.apiKey,
        apiBaseUrl: options.apiBaseUrl,
      });

      const [org, agents] = await Promise.all([
        client.getOrg(),
        client.listOrgAgents(),
      ]);

      const sortedAgents = [...agents].sort(
        (left, right) =>
          left.name.localeCompare(right.name) || left.id.localeCompare(right.id),
      );

      if (options.json) {
        console.log(
          JSON.stringify(
            {
              org: {
                id: org.id,
                name: org.name,
              },
              agents: sortedAgents,
            },
            null,
            2,
          ),
        );
        return;
      }

      if (sortedAgents.length === 0) {
        logInfo("No agents found in this organization.");
        return;
      }

      logInfo(`Found ${sortedAgents.length} agent(s) in ${org.name}:\n`);
      for (const agent of sortedAgents) {
        console.log(
          `  ${COLORS.bold}${agent.name}${COLORS.reset}  ${COLORS.dim}${agent.id}${COLORS.reset}`,
        );
      }
    }),
  );

export const modelAgentCommand = new Command("agent")
  .description("List and inspect organization agents")
  .addCommand(modelAgentListCommand);
