export function parseJsonStrings<T>(value: T): T {
  return parseJsonRecursively(value) as T;
}

export function flattenObjectKeys(
  input: Record<string, unknown> | undefined
): Record<string, unknown> | undefined {
  if (!input) return input;
  return flattenRecursively(input, "");
}

function parseJsonRecursively(value: any): any {
  if (
    value == null ||
    typeof value !== "object" ||
    value instanceof Date ||
    value instanceof Buffer
  ) {
    if (typeof value === "string") {
      const trimmed = value.trim();
      if (
        (trimmed.startsWith("{") && trimmed.endsWith("}")) ||
        (trimmed.startsWith("[") && trimmed.endsWith("]"))
      ) {
        try {
          return parseJsonRecursively(JSON.parse(trimmed));
        } catch {
          return value;
        }
      }
    }
    return value;
  }

  if (Array.isArray(value)) {
    return value.map((entry) => parseJsonRecursively(entry));
  }

  const result: Record<string, unknown> = {};
  for (const [key, entry] of Object.entries(value)) {
    result[key] = parseJsonRecursively(entry);
  }
  return result;
}

function flattenRecursively(
  value: Record<string, unknown>,
  prefix: string
): Record<string, unknown> {
  const result: Record<string, unknown> = {};
  for (const [key, entry] of Object.entries(value)) {
    const nextKey = prefix ? `${prefix}.${key}` : key;
    if (isPlainRecord(entry)) {
      Object.assign(result, flattenRecursively(entry, nextKey));
    } else {
      result[nextKey] = entry;
    }
  }
  return result;
}

function isPlainRecord(value: unknown): value is Record<string, unknown> {
  return (
    typeof value === "object" &&
    value !== null &&
    !Array.isArray(value)
  );
}
