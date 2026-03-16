export type OptionRecord = Record<string, unknown>;

export function optionRecord(raw: unknown): OptionRecord {
  if (!raw || typeof raw !== "object") {
    return {};
  }
  return raw as OptionRecord;
}

export function parseString(value: unknown): string | undefined {
  if (typeof value !== "string") {
    return undefined;
  }
  const trimmed = value.trim();
  return trimmed.length > 0 ? trimmed : undefined;
}

export function parseBoolean(value: unknown): boolean {
  return value === true;
}
