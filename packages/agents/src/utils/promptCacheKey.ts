import crypto from "crypto";

export type CacheKeyInput = {
  agentId: string | number;
  userContext?: Record<string, string | number>;
};

function hashContext(context: Record<string, string | number>): string {
  const flattened = Object.keys(context)
    .sort()
    .map((key) => `${key}:${String(context[key])}`)
    .join("|");
  return crypto.createHash("sha256").update(flattened).digest("hex").slice(0, 16);
}

/**
 * Build a stable per-user cache key for OpenAI prompt caching.
 * Reuse the same key across prompts/steps so shared prefixes can cache together.
 */
export function buildPromptCacheKey({
  agentId,
  userContext,
}: CacheKeyInput): string {
  if (!userContext || Object.keys(userContext).length === 0) {
    return `${agentId}:noctx`;
  }

  const contextHash = hashContext(userContext);
  return `${agentId}:${contextHash}`;
}
