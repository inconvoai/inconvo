type OptionRecord = Record<string, unknown>;

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
  json: boolean;
}

function optionRecord(raw: unknown): OptionRecord {
  if (typeof raw !== "object" || raw === null) {
    return {};
  }

  const parsed: OptionRecord = {};
  for (const [key, value] of Object.entries(raw)) {
    parsed[key] = value;
  }
  return parsed;
}

function parseString(value: unknown): string | undefined {
  if (typeof value !== "string") {
    return undefined;
  }
  const trimmed = value.trim();
  return trimmed.length > 0 ? trimmed : undefined;
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

function parseBoolean(value: unknown): boolean {
  return value === true;
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
