import * as p from "@clack/prompts";
import type { AgentInfo } from "./types.js";

export interface InteractivePullSelection {
  selectedAgentIds: string[];
}

function formatAgentLabel(agent: AgentInfo): string {
  return `${agent.name} (${agent.id})`;
}

export async function promptPullSelection(
  agents: AgentInfo[],
): Promise<InteractivePullSelection> {
  if (agents.length === 0) {
    throw new Error("No agents available in this organization.");
  }

  const mode = await p.select({
    message: "Choose pull target",
    options: [
      {
        label: "One agent",
        value: "one",
        hint: "Select a single agent",
      },
      {
        label: "Many agents",
        value: "many",
        hint: "Select a subset",
      },
      {
        label: "Pull all agents",
        value: "all",
        hint: "Use all current org agents",
      },
    ],
  });

  if (p.isCancel(mode)) {
    throw new Error("Pull cancelled.");
  }

  let selectedAgentIds: string[] = [];
  if (mode === "one") {
    const selected = await p.select({
      message: "Select agent",
      options: agents.map((agent) => ({
        label: formatAgentLabel(agent),
        value: agent.id,
      })),
    });
    if (p.isCancel(selected)) {
      throw new Error("Pull cancelled.");
    }
    selectedAgentIds = [selected];
  } else if (mode === "many") {
    const selected = await p.multiselect({
      message: "Select agents",
      options: agents.map((agent) => ({
        label: formatAgentLabel(agent),
        value: agent.id,
      })),
      required: true,
    });
    if (p.isCancel(selected)) {
      throw new Error("Pull cancelled.");
    }
    selectedAgentIds = selected;
  } else {
    selectedAgentIds = agents.map((agent) => agent.id);
  }

  return {
    selectedAgentIds,
  };
}
