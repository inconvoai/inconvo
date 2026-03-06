export function nonEmptyString(value: unknown): string | null {
  if (typeof value !== "string") {
    return null;
  }
  const trimmed = value.trim();
  return trimmed.length > 0 ? trimmed : null;
}

export function fallbackMessage(
  message: string | null | undefined,
  fallback: string,
): string {
  const normalized = nonEmptyString(message);
  return normalized ?? fallback;
}

export function errorMessageWithFallback(
  error: unknown,
  fallback: string,
): string {
  if (error instanceof Error) {
    return fallbackMessage(error.message, fallback);
  }
  return fallback;
}
