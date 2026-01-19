import fs from "fs/promises";
import path from "path";
import crypto from "crypto";
import {
  unifiedAugmentationSchema,
  type UnifiedAugmentation,
} from "../types/customSchema";
import { logger } from "./logger";

const schemaAugmentationsRoot =
  process.env.SCHEMA_AUGMENTATIONS_DIR ||
  path.resolve(process.cwd(), "schema-augmentations");

const SCHEMA_AUGMENTATIONS_DIR = path.resolve(schemaAugmentationsRoot);

const AUGMENTATIONS_FILE = path.join(
  SCHEMA_AUGMENTATIONS_DIR,
  "augmentations.json",
);

async function ensureSchemaAugmentationsDir() {
  await fs.mkdir(SCHEMA_AUGMENTATIONS_DIR, { recursive: true });
}

/**
 * Normalize an object to have consistent key ordering.
 * This ensures the same data produces the same JSON regardless of original key order.
 */
function normalizeObject(obj: unknown): unknown {
  if (obj === null || typeof obj !== "object") {
    return obj;
  }
  if (Array.isArray(obj)) {
    return obj.map(normalizeObject);
  }
  // Sort keys alphabetically and recursively normalize values
  const sorted: Record<string, unknown> = {};
  for (const key of Object.keys(obj).sort()) {
    sorted[key] = normalizeObject((obj as Record<string, unknown>)[key]);
  }
  return sorted;
}

/**
 * Compute a deterministic SHA-256 hash of augmentations.
 * Arrays are sorted by deterministic keys for consistent hashing.
 * Objects are normalized to have consistent key ordering.
 */
export function computeAugmentationsHash(payload: {
  relations: unknown[];
  computedColumns: unknown[];
  columnConversions: unknown[];
}): string {
  const sorted = {
    relations: [...payload.relations]
      .sort((a: any, b: any) =>
        `${a.sourceTable}.${a.name}`.localeCompare(
          `${b.sourceTable}.${b.name}`,
        ),
      )
      .map(normalizeObject),
    computedColumns: [...payload.computedColumns]
      .sort((a: any, b: any) =>
        `${a.table}.${a.name}`.localeCompare(`${b.table}.${b.name}`),
      )
      .map(normalizeObject),
    columnConversions: [...payload.columnConversions]
      .sort((a: any, b: any) =>
        `${a.table}.${a.column}`.localeCompare(`${b.table}.${b.column}`),
      )
      .map(normalizeObject),
  };
  const canonical = JSON.stringify(sorted);
  return crypto.createHash("sha256").update(canonical).digest("hex");
}

/**
 * Write unified augmentations to a single file.
 */
export async function writeUnifiedAugmentation(
  augmentation: UnifiedAugmentation,
): Promise<void> {
  const payload = unifiedAugmentationSchema.parse(augmentation);
  await ensureSchemaAugmentationsDir();
  const tmpPath = `${AUGMENTATIONS_FILE}.tmp`;
  await fs.writeFile(tmpPath, JSON.stringify(payload, null, 2), "utf8");
  await fs.rename(tmpPath, AUGMENTATIONS_FILE);
}

/**
 * Read unified augmentations from the single file.
 * Returns empty augmentations if file doesn't exist.
 */
export async function readUnifiedAugmentation(): Promise<UnifiedAugmentation> {
  const empty: UnifiedAugmentation = {
    relations: [],
    computedColumns: [],
    columnConversions: [],
  };

  try {
    const contents = await fs.readFile(AUGMENTATIONS_FILE, "utf8");
    const parsed = unifiedAugmentationSchema.safeParse(JSON.parse(contents));
    if (parsed.success) {
      return parsed.data;
    }
    logger.warn(
      { issues: parsed.error.issues },
      "Schema augmentation store - invalid unified augmentation file",
    );
    return empty;
  } catch (error: any) {
    if (error.code !== "ENOENT") {
      logger.warn(
        { error },
        "Schema augmentation store - failed to read unified augmentation file",
      );
    }
    return empty;
  }
}

/**
 * Get the hash of the current local augmentations.
 */
export async function getLocalAugmentationsHash(): Promise<string> {
  const augmentation = await readUnifiedAugmentation();
  return computeAugmentationsHash(augmentation);
}
