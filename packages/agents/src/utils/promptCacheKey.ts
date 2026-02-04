import crypto from "crypto";

export type CacheKeyInput = {
  agentId: string | number;
  userIdentifier?: string | null;
};

const MAX_CACHE_KEY_LENGTH = 64;
const HASH_LENGTH = 16;

function hashIdentifier(identifier: string): string {
  return crypto
    .createHash("sha256")
    .update(identifier)
    .digest("hex")
    .slice(0, HASH_LENGTH);
}

/**
 * Build a stable per-user cache key for OpenAI prompt caching.
 * Reuse the same key across prompts/steps so shared prefixes can cache together.
 */
export function buildPromptCacheKey({
  agentId,
  userIdentifier,
}: CacheKeyInput): string {
  if (!userIdentifier) {
    return `${agentId}:nouser`;
  }

  const rawKey = `${agentId}:${userIdentifier}`;
  if (rawKey.length <= MAX_CACHE_KEY_LENGTH) {
    return rawKey;
  }

  const hashed = hashIdentifier(userIdentifier);
  const shortKey = `${agentId}:u_${hashed}`;
  if (shortKey.length <= MAX_CACHE_KEY_LENGTH) {
    return shortKey;
  }

  return `k_${hashIdentifier(rawKey)}`;
}
