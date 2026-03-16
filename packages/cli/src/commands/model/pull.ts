import { Command } from "commander";
import { isInteractiveEnvironment, DEFAULT_API_BASE_URL } from "../../model/cli-options.js";
import {
  resolvePullTargetMode,
} from "../../model/selection.js";
import { promptPullSelection } from "../../model/prompts.js";
import {
  pullAgentsToWorkspace,
} from "../../model/operations.js";
import { logInfo } from "../../process/output.js";
import type { AgentInfo } from "../../model/types.js";
import { runCliAction } from "../_shared/command-runtime.js";
import { resolveModelPullContext } from "../_shared/model-command-factory.js";

function collectAgentId(value: string, previous: string[]): string[] {
  return [...previous, value];
}

function resolveAgentsById(
  allAgents: AgentInfo[],
  requestedIds: string[],
): AgentInfo[] {
  const agentById = new Map(allAgents.map((agent) => [agent.id, agent] as const));
  const missing = requestedIds.filter((agentId) => !agentById.has(agentId));
  if (missing.length > 0) {
    throw new Error(
      `Unknown agent id(s): ${missing.join(", ")}. Run with --all-agents to list all available agents first.`,
    );
  }
  return requestedIds.map((agentId) => agentById.get(agentId)!);
}

export const modelPullCommand = new Command("pull")
  .description("Pull semantic-model files into .inconvo/ for one or more agents")
  .option(
    "--agent <agentId>",
    "Target agent id (repeatable)",
    collectAgentId,
    [],
  )
  .option("--all-agents", "Pull all agents in the organization")
  .option(
    "--connection <connectionId>",
    "Restrict pull to one connection (requires exactly one --agent)",
  )
  .option("--json", "Print JSON output")
  .option("--api-key <apiKey>", "API key override (otherwise INCONVO_API_KEY)")
  .option(
    "--api-base-url <url>",
    `API base URL override (default: ${DEFAULT_API_BASE_URL})`,
  )
  .addHelpText(
    "after",
    [
      "",
      "Selector rules:",
      "  - In non-interactive mode, pull requires exactly one of --agent or --all-agents.",
      "  - --connection requires exactly one --agent.",
    ].join("\n"),
  )
  .action((options) =>
    runCliAction(async () => {
      const { parsedOptions, agentIds, repoRoot, client } =
        await resolveModelPullContext(options);
      const org = await client.getOrg();
      const targetMode = resolvePullTargetMode({
        input: {
          agentIds,
          allAgents: parsedOptions.allAgents,
          connectionId: parsedOptions.connectionId,
        },
        interactive: isInteractiveEnvironment(),
      });

      if (targetMode.kind === "error") {
        throw new Error(targetMode.message);
      }

      const allAgents = await client.listOrgAgents();
      let selectedAgents: AgentInfo[] = [];

      if (targetMode.kind === "all_agents") {
        selectedAgents = allAgents;
      } else if (targetMode.kind === "explicit_agents") {
        selectedAgents = resolveAgentsById(allAgents, targetMode.agentIds);
      } else {
        const selection = await promptPullSelection(allAgents);
        selectedAgents = resolveAgentsById(allAgents, selection.selectedAgentIds);
      }

      if (selectedAgents.length === 0) {
        throw new Error("No agents selected for pull.");
      }

      const selectedConnectionId = parsedOptions.connectionId;
      if (selectedConnectionId && selectedAgents.length !== 1) {
        throw new Error("--connection requires exactly one --agent.");
      }

      const result = await pullAgentsToWorkspace({
        client,
        repoRoot,
        org,
        selectedAgents,
        selectedConnectionId,
      });

      if (parsedOptions.json) {
        console.log(
          JSON.stringify(
            {
              org: { id: org.id, name: org.name },
              targetMode: targetMode.kind,
              selectedAgentIds: selectedAgents.map((agent) => agent.id),
              selectedConnectionId,
              result,
            },
            null,
            2,
          ),
        );
        return;
      }

      logInfo(
        `Pull complete: ${result.pulledAgents} agent(s), ${result.pulledConnections} connection snapshot(s).`,
      );
    }),
  );
