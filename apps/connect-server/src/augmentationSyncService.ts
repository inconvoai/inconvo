import axios from "axios";
import { env } from "./env";
import {
  readUnifiedAugmentation,
  writeUnifiedAugmentation,
  computeAugmentationsHash,
  clearAugmentedSchemaCache,
} from "@repo/connect";
import { unifiedAugmentationsSyncSchema } from "@repo/types";
import pino from "pino";

const logger = pino({ name: "augmentation-sync" });

// Cached hash to avoid recomputing on every request
let cachedHash: string | null = null;

/**
 * Get the hash of local augmentations (cached).
 */
export async function getLocalHash(): Promise<string> {
  if (cachedHash) {
    return cachedHash;
  }
  const augmentation = await readUnifiedAugmentation();
  cachedHash = computeAugmentationsHash(augmentation);
  return cachedHash;
}

/**
 * Clear the cached hash when augmentations are updated.
 */
export function clearLocalHashCache(): void {
  cachedHash = null;
}

export type SyncResult = {
  synced: boolean;
  error?: string;
};

/**
 * Validate augmentations hash and sync from platform if there's a mismatch.
 */
export async function validateAndSyncAugmentations(
  incomingHash: string,
): Promise<SyncResult> {
  const localHash = await getLocalHash();

  // Already in sync
  if (localHash === incomingHash) {
    return { synced: false };
  }

  logger.info(
    {
      localHash: localHash.slice(0, 8),
      incomingHash: incomingHash.slice(0, 8),
    },
    "Augmentations hash mismatch, fetching fresh data from platform",
  );

  try {
    const url = `${env.INCONVO_PLATFORM_URL}/api/v1/agents/${env.INCONVO_AGENT_ID}/augmentations`;
    logger.debug({ url }, "Fetching augmentations from platform");

    const response = await axios.get(url, {
      headers: {
        Authorization: `Bearer ${env.INCONVO_API_KEY}`,
      },
      timeout: 10000,
    });

    // Log response details for debugging
    if (typeof response.data === "string") {
      logger.error(
        {
          status: response.status,
          contentType: response.headers["content-type"],
          dataPreview: response.data.slice(0, 200),
        },
        "Platform returned string instead of JSON object",
      );
      return {
        synced: false,
        error: `Platform returned invalid response: ${response.data.slice(0, 100)}`,
      };
    }

    const payload = unifiedAugmentationsSyncSchema.parse(response.data);

    // Verify the fetched hash matches what we expected
    if (payload.hash !== incomingHash) {
      logger.warn(
        {
          expectedHash: incomingHash.slice(0, 8),
          receivedHash: payload.hash.slice(0, 8),
        },
        "Fetched augmentations hash doesn't match expected - data may have changed during sync",
      );
    }

    await writeUnifiedAugmentation({
      updatedAt: payload.updatedAt,
      hash: payload.hash,
      relations: payload.relations,
      computedColumns: payload.computedColumns,
      columnConversions: payload.columnConversions,
    });

    clearLocalHashCache();
    clearAugmentedSchemaCache();

    logger.info(
      { hash: payload.hash.slice(0, 8) },
      "Augmentations synced successfully from platform",
    );
    return { synced: true };
  } catch (error) {
    // Handle axios errors with response details
    if (axios.isAxiosError(error) && error.response) {
      const { status, data, headers } = error.response;
      logger.error(
        {
          status,
          contentType: headers["content-type"],
          data: typeof data === "string" ? data.slice(0, 200) : data,
        },
        "Platform API request failed",
      );
      return {
        synced: false,
        error: `Platform API error ${status}: ${typeof data === "object" ? JSON.stringify(data) : String(data).slice(0, 100)}`,
      };
    }

    const message = error instanceof Error ? error.message : "Unknown error";
    logger.error({ error }, "Failed to sync augmentations from platform");
    return { synced: false, error: message };
  }
}
