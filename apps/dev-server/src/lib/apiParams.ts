/**
 * Parse and validate query parameters for listing conversations
 */
export function parseListParams(searchParams: URLSearchParams) {
  const cursor = searchParams.get("cursor") ?? undefined;
  const limitStr = searchParams.get("limit");
  const userIdentifier = searchParams.get("userIdentifier") ?? undefined;

  // Parse limit with default
  let limit = 20;
  if (limitStr) {
    const parsed = parseInt(limitStr, 10);
    if (!isNaN(parsed) && parsed >= 1 && parsed <= 100) {
      limit = parsed;
    }
  }

  // Parse userContext parameters (e.g., userContext[orgId]=123)
  const userContext: Record<string, string | number> = {};
  for (const [key, value] of searchParams.entries()) {
    const match = key.match(/^userContext\[(.+)\]$/);
    if (match && match[1]) {
      const contextKey = match[1];
      // Try to parse as number, otherwise keep as string
      const numValue = Number(value);
      userContext[contextKey] = isNaN(numValue) ? value : numValue;
    }
  }

  return {
    cursor,
    limit,
    userIdentifier,
    userContext: Object.keys(userContext).length > 0 ? userContext : undefined,
  };
}

/**
 * Encode cursor for pagination (base64 encoded timestamp)
 */
export function encodeCursor(date: Date): string {
  return Buffer.from(date.toISOString()).toString("base64");
}

/**
 * Decode cursor from pagination
 */
export function decodeCursor(cursor: string): Date | null {
  try {
    const decoded = Buffer.from(cursor, "base64").toString("utf-8");
    const date = new Date(decoded);
    return isNaN(date.getTime()) ? null : date;
  } catch {
    return null;
  }
}
