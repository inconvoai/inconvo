export interface PullFlagInput {
  agentIds: string[];
  allAgents: boolean;
  connectionId?: string;
}

export type PullTargetMode =
  | { kind: "explicit_agents"; agentIds: string[] }
  | { kind: "all_agents" }
  | { kind: "interactive" }
  | { kind: "error"; message: string };

export function normalizeAgentIds(agentIds: string[]): string[] {
  return [...new Set(agentIds.map((id) => id.trim()).filter((id) => id !== ""))];
}

export function validatePullFlags(input: PullFlagInput): string | null {
  if (input.allAgents && input.agentIds.length > 0) {
    return "--all-agents cannot be combined with --agent.";
  }

  if (input.connectionId && input.allAgents) {
    return "--connection cannot be combined with --all-agents.";
  }

  if (input.connectionId && input.agentIds.length !== 1) {
    return "--connection requires exactly one --agent.";
  }

  return null;
}

export function resolvePullTargetMode(params: {
  input: PullFlagInput;
  interactive: boolean;
}): PullTargetMode {
  const normalizedAgentIds = normalizeAgentIds(params.input.agentIds);

  if (normalizedAgentIds.length > 0) {
    return { kind: "explicit_agents", agentIds: normalizedAgentIds };
  }

  if (params.input.allAgents) {
    return { kind: "all_agents" };
  }

  if (params.interactive) {
    return { kind: "interactive" };
  }

  return {
    kind: "error",
    message:
      "No pull target resolved. In non-interactive mode, pass --agent (repeatable) or --all-agents.",
  };
}
