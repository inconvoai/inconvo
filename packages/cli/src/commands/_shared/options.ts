import {
  optionRecord,
  parseString,
  parseBoolean,
  type OptionRecord,
} from "../../model/parse-utils.js";

export interface ApiCommandOptions {
  apiBaseUrl?: string;
  apiKey?: string;
}

export interface ModelPullCommandOptions extends ApiCommandOptions {
  agentIds: string[];
  allAgents: boolean;
  connectionId?: string;
  json: boolean;
}

export interface ConnectionCommandOptions extends ApiCommandOptions {
  agentId?: string;
  connectionId?: string;
  description?: string;
  clearDescription: boolean;
  json: boolean;
}

function parseStringList(value: unknown): string[] {
  if (Array.isArray(value)) {
    const parsed: string[] = [];
    for (const entry of value) {
      const normalized = parseString(entry);
      if (normalized) {
        parsed.push(normalized);
      }
    }
    return parsed;
  }

  const single = parseString(value);
  return single ? [single] : [];
}

function parseApiCommandOptions(record: OptionRecord): ApiCommandOptions {
  return {
    apiBaseUrl: parseString(record.apiBaseUrl),
    apiKey: parseString(record.apiKey),
  };
}

export function parseModelPullCommandOptions(
  raw: unknown,
): ModelPullCommandOptions {
  const record = optionRecord(raw);
  return {
    ...parseApiCommandOptions(record),
    agentIds: parseStringList(record.agent),
    allAgents: parseBoolean(record.allAgents),
    connectionId: parseString(record.connection),
    json: parseBoolean(record.json),
  };
}

export function parseConnectionCommandOptions(
  raw: unknown,
): ConnectionCommandOptions {
  const record = optionRecord(raw);
  return {
    ...parseApiCommandOptions(record),
    agentId: parseString(record.agent),
    connectionId: parseString(record.connection),
    description: parseString(record.description),
    clearDescription: parseBoolean(record.clearDescription),
    json: parseBoolean(record.json),
  };
}

export function requireConnectionInNonInteractiveMode(params: {
  connectionId?: string;
  interactive: boolean;
}): string | undefined {
  if (params.connectionId) {
    return params.connectionId;
  }

  if (!params.interactive) {
    throw new Error("--connection is required in non-interactive mode.");
  }

  return undefined;
}
