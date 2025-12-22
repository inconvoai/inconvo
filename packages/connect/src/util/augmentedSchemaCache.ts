import type { SchemaResponse } from "../types/types";
import { buildAugmentedSchema } from "./buildAugmentedSchema";

let augmentedSchemaCache: SchemaResponse | null = null;
let augmentedSchemaPromise: Promise<SchemaResponse> | null = null;

export async function getAugmentedSchema(): Promise<SchemaResponse> {
  if (augmentedSchemaCache) {
    return augmentedSchemaCache;
  }
  if (augmentedSchemaPromise) {
    return augmentedSchemaPromise;
  }
  augmentedSchemaPromise = buildAugmentedSchema();
  try {
    augmentedSchemaCache = await augmentedSchemaPromise;
    return augmentedSchemaCache;
  } finally {
    augmentedSchemaPromise = null;
  }
}

export function clearAugmentedSchemaCache() {
  augmentedSchemaCache = null;
  augmentedSchemaPromise = null;
}
