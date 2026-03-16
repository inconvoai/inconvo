export function parseBooleanArg(raw: string): boolean {
  const value = raw.trim().toLowerCase();
  if (["true", "1", "yes", "y"].includes(value)) {
    return true;
  }
  if (["false", "0", "no", "n"].includes(value)) {
    return false;
  }
  throw new Error(`Invalid boolean value "${raw}". Use true/false.`);
}

export function parseIntArg(raw: string): number {
  const trimmed = raw.trim();
  if (!/^-?\d+$/.test(trimmed)) {
    throw new Error(`Invalid integer value "${raw}".`);
  }

  const parsed = Number(trimmed);
  if (!Number.isSafeInteger(parsed)) {
    throw new Error(`Invalid integer value "${raw}".`);
  }
  return parsed;
}

export function parseJsonOption(raw: string, label: string): unknown {
  try {
    return JSON.parse(raw) as unknown;
  } catch {
    throw new Error(`Invalid JSON for ${label}.`);
  }
}

export function collectPair(value: string, previous: string[]): string[] {
  return [...previous, value];
}

export function parseColumnPairs(values: string[]): Array<{
  sourceColumnName: string;
  targetColumnName: string;
}> {
  const pairs = values.map((value) => {
    const split = value.split(":");
    if (split.length !== 2) {
      throw new Error(
        `Invalid pair "${value}". Use "sourceColumn:targetColumn".`,
      );
    }

    const source = split[0]?.trim() ?? "";
    const target = split[1]?.trim() ?? "";
    if (source.length === 0 || target.length === 0) {
      throw new Error(
        `Invalid pair "${value}". Use "sourceColumn:targetColumn".`,
      );
    }
    return {
      sourceColumnName: source,
      targetColumnName: target,
    };
  });

  if (pairs.length === 0) {
    throw new Error("At least one --pair is required.");
  }

  return pairs;
}
