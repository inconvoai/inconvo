const DEFAULT_WINDOW_SECONDS = 300;

interface NonceRecord {
  timestamp: number;
  seenAt: number;
}

const nonceCache = new Map<string, NonceRecord>();

function purgeExpired(nowSeconds: number, windowSeconds: number) {
  for (const [nonce, record] of nonceCache) {
    if (nowSeconds - record.seenAt > windowSeconds) {
      nonceCache.delete(nonce);
    }
  }
}

export function registerNonce(
  nonce: string,
  requestTimestamp: number,
  options?: {
    nowSeconds?: number;
    windowSeconds?: number;
  }
): boolean {
  const windowSeconds = options?.windowSeconds ?? DEFAULT_WINDOW_SECONDS;
  const nowSeconds = options?.nowSeconds ?? Math.floor(Date.now() / 1000);

  purgeExpired(nowSeconds, windowSeconds);

  const existing = nonceCache.get(nonce);
  if (existing !== undefined) {
    return false;
  }

  nonceCache.set(nonce, { timestamp: requestTimestamp, seenAt: nowSeconds });
  return true;
}

export function resetNonceCache() {
  nonceCache.clear();
}
