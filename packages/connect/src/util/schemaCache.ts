import { SchemaResponse } from "~/types/types";
import { buildSchema } from "./buildSchema";
import { logSchemaPreloadFailureHint } from "./databaseDiagnostics";

// Global schema cache
let schemaCache: SchemaResponse | null = null;
let schemaPromise: Promise<SchemaResponse> | null = null;

/**
 * Gets the schema, using cache if available.
 * If multiple requests come in while the schema is being built,
 * they'll all wait for the same promise to resolve.
 */
export async function getCachedSchema(): Promise<SchemaResponse> {
  // If we have a cached schema, return it
  if (schemaCache) {
    return schemaCache;
  }

  // If we're already building the schema, wait for it
  if (schemaPromise) {
    return schemaPromise;
  }

  // Build the schema and cache it
  schemaPromise = buildSchema();

  try {
    schemaCache = await schemaPromise;
    return schemaCache;
  } finally {
    schemaPromise = null;
  }
}

/**
 * Clears the schema cache, forcing a rebuild on next access
 */
export function clearSchemaCache(): void {
  schemaCache = null;
  schemaPromise = null;
}

/**
 * Pre-loads the schema into cache
 */
export async function preloadSchema(): Promise<void> {
  try {
    await getCachedSchema();
  } catch (error) {
    logSchemaPreloadFailureHint(error);
    throw error;
  }
}
