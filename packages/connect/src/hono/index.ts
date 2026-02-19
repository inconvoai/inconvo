import { Hono } from "hono";
import { sql, type Kysely } from "kysely";
import { ZodError } from "zod";
import { SchemaResponseSchema } from "@repo/types";
import { createHmacMiddleware } from "./middleware/hmac";
import { QuerySchema } from "../types/querySchema";
import { aggregate } from "../operations/aggregate";
import { aggregateGroups } from "../operations/aggregateGroups";
import { findMany } from "../operations/findMany";
import { count } from "../operations/count";
import { countRelations } from "../operations/countRelations";
import { groupBy } from "../operations/groupBy";
import { findDistinct } from "../operations/findDistinct";
import { findDistinctByEditDistance } from "../operations/findDistinctByEditDistance";
import { unifiedAugmentationSchema } from "../types/customSchema";
import type { SchemaTable, SchemaResponse } from "../types/types";
import type { DatabaseDialect, OperationContext } from "../operations/types";
import { QueryExecutionError } from "../util/queryErrors";

/**
 * Logger interface for structured logging.
 * Implementations should include connectionId in all log output.
 */
export interface Logger {
  debug(message: string, extra?: Record<string, unknown>): void;
  info(message: string, extra?: Record<string, unknown>): void;
  warn(message: string, extra?: Record<string, unknown>): void;
  error(message: string, error?: unknown, extra?: Record<string, unknown>): void;
  timing(message: string, durationMs: number, extra?: Record<string, unknown>): void;
}

/**
 * Default no-op logger for when no logger is provided.
 */
const noopLogger: Logger = {
  debug: () => {},
  info: () => {},
  warn: () => {},
  error: () => {},
  timing: () => {},
};

/**
 * Dependencies required by the Lambda app.
 * These are injected by the Lambda app to keep the factory reusable and testable.
 */
export interface LambdaDeps {
  /**
   * Introspect the database schema for a connection.
   * Called on GET / to fetch fresh schema from the database.
   * If provided, GET / will introspect and write to storage.
   * If not provided, GET / will read from storage (legacy behavior).
   */
  introspectSchema?: (connectionId: string) => Promise<SchemaResponse>;

  /**
   * Load schema, augmentations, database connection, and dialect for a given connection ID.
   * Used by POST / for query execution.
   * The Lambda app implements this by fetching from S3 and Secrets Manager.
   */
  loadSchemaAndAugmentations: (connectionId: string) => Promise<{
    schema: SchemaResponse;
    db: Kysely<unknown>;
    dialect: DatabaseDialect;
  }>;

  /**
   * Get the signing key for HMAC validation.
   * The Lambda app implements this by fetching from Secrets Manager.
   */
  getSigningKey: (connectionId: string) => Promise<string>;

  /**
   * Optional: Create a logger for a connection.
   * If not provided, logs will use console.log without structured formatting.
   */
  createLogger?: (connectionId: string) => Logger;

  /**
   * Optional: Write schema to storage (S3).
   * Called after introspection to persist the schema.
   * Required if introspectSchema is provided.
   */
  writeSchema?: (connectionId: string, schema: SchemaResponse) => Promise<void>;

  /**
   * Optional: Write augmentations to storage (S3).
   * Required for /sync/augmentations endpoint.
   */
  writeAugmentations?: (connectionId: string, augmentations: unknown) => Promise<void>;

  /**
   * Optional: Clear cached schema/augmentations for a connection.
   * Called after sync to ensure fresh data is loaded on next request.
   */
  clearCache?: (connectionId: string) => void;

  /**
   * Optional: Clear all caches for a connection (schema, secrets, DB connections).
   * Used for immediate cache invalidation after credential rotation.
   */
  clearAllCaches?: (connectionId: string) => void;

  /**
   * Optional: Invalidate caches when the connection version increases.
   */
  invalidateCacheIfStale?: (
    connectionId: string,
    requestVersion: number,
  ) => Promise<void> | void;

  /**
   * Optional: Validate augmentations hash parity before executing queries.
   * If the expected hash is missing, no validation should occur.
   * If validation fails, this should throw to block the request.
   */
  validateAugmentationsHash?: (
    connectionId: string,
    expectedHash?: string,
  ) => Promise<void>;

  /**
   * Optional: Version string to return from GET /version.
   * If not provided, returns "unknown".
   */
  version?: string;
}

/**
 * Safely stringify JSON, converting BigInt to string.
 */
function safeJsonStringify(value: unknown): string {
  return JSON.stringify(value, (_key, val) => {
    if (typeof val === "bigint") {
      return val.toString();
    }
    return val;
  });
}

/**
 * Sanitize schema for public API response.
 * Removes internal fields that shouldn't be exposed.
 */
function sanitizeSchema(tables: SchemaTable[]) {
  return tables.map(
    ({
      schema: _tableSchema,
      computedColumns: _computedColumns,
      columnConversions: _columnConversions,
      ...table
    }) => ({
      ...table,
      columns: table.columns.map(
        ({
          isStructField: _isStructField,
          structParent: _structParent,
          structFieldPath: _structFieldPath,
          ...column
        }) => column,
      ),
    }),
  );
}

/**
 * Create a Hono app for the Lambda handler.
 * Accepts dependencies to keep the app testable and decoupled from infrastructure.
 */
export function createApp(deps: LambdaDeps) {
  const getLogger = (connectionId: string): Logger => {
    return deps.createLogger?.(connectionId) ?? noopLogger;
  };

  const app = new Hono<{
    Variables: {
      connectionId: string;
      parsedBody?: unknown;
      logger: Logger;
    };
  }>();

  // Health check - no auth required
  app.get("/healthz", (c) => {
    return c.json({ ok: true });
  });

  // Version - no auth required
  app.get("/version", (c) => {
    return c.json({ version: deps.version ?? "unknown" });
  });

  // HMAC signature validation for all other routes
  app.use("/*", createHmacMiddleware(deps.getSigningKey));

  // Attach logger after HMAC middleware sets connectionId
  app.use("/*", async (c, next) => {
    const connectionId = c.get("connectionId");
    const logger = getLogger(connectionId);
    c.set("logger", logger);
    await next();
  });

  // Invalidate caches when the connection version increases
  app.use("/*", async (c, next) => {
    if (deps.invalidateCacheIfStale) {
      const connectionId = c.get("connectionId");
      const versionHeader = c.req.header("inconvo-connection-version");
      const requestVersion = versionHeader ? Number(versionHeader) : NaN;
      if (Number.isFinite(requestVersion)) {
        await deps.invalidateCacheIfStale(connectionId, requestVersion);
      }
    }
    await next();
  });

  // Database health check - requires auth (needs connection ID to know which DB)
  app.get("/healthz/db", async (c) => {
    const connectionId = c.get("connectionId");
    const logger = c.get("logger");
    const startTime = Date.now();

    try {
      const { db, dialect } = await deps.loadSchemaAndAugmentations(connectionId);

      // Run a simple query to verify DB connectivity (works across all dialects)
      await sql.raw("SELECT 1 AS ok").execute(db);

      const duration = Date.now() - startTime;
      logger.timing("Database health check passed", duration);

      return c.json({ ok: true, dialect, durationMs: duration });
    } catch (error) {
      const duration = Date.now() - startTime;
      logger.error("Database health check failed", error, { durationMs: duration });

      const message = error instanceof Error ? error.message : "Unknown error";
      return c.json({ ok: false, error: message, durationMs: duration }, 503);
    }
  });

  // Get schema - introspects database and writes to storage
  app.get("/", async (c) => {
    const connectionId = c.get("connectionId");
    const logger = c.get("logger");
    const startTime = Date.now();

    try {
      let schema: SchemaResponse;

      // If introspectSchema is provided, introspect and write to storage
      if (deps.introspectSchema) {
        logger.info("Introspecting database schema");
        schema = await deps.introspectSchema(connectionId);

        // Write to storage for caching (used by POST / queries)
        if (deps.writeSchema) {
          await deps.writeSchema(connectionId, schema);
          deps.clearCache?.(connectionId);
          logger.info("Schema written to storage");
        }
      } else {
        // Fallback: read from storage (legacy behavior)
        logger.info("Fetching schema from storage");
        const result = await deps.loadSchemaAndAugmentations(connectionId);
        schema = result.schema;
      }

      const sanitizedTables = sanitizeSchema(schema.tables);

      logger.timing("Schema fetched successfully", Date.now() - startTime, {
        tableCount: sanitizedTables.length,
      });

      return c.json({
        tables: sanitizedTables,
        databaseSchemas: schema.databaseSchemas,
      });
    } catch (error) {
      logger.error("Failed to fetch schema", error);
      return c.json({ error: "Failed to fetch schema" }, 500);
    }
  });

  // Sync schema from Platform
  app.post("/sync/schema", async (c) => {
    const connectionId = c.get("connectionId");
    const logger = c.get("logger");
    const body = c.get("parsedBody");

    if (!deps.writeSchema) {
      logger.error("writeSchema not configured");
      return c.json({ error: "Schema sync not supported" }, 501);
    }

    try {
      const schema = SchemaResponseSchema.parse(body);
      logger.info("Syncing schema", { tableCount: schema.tables.length });
      await deps.writeSchema(connectionId, schema);
      deps.clearCache?.(connectionId);
      logger.info("Schema synced successfully");
      return c.json({ ok: true });
    } catch (error) {
      if (error instanceof ZodError) {
        logger.warn("Invalid schema payload", { issues: error.issues });
        return c.json({ error: "Invalid schema payload" }, 400);
      }
      logger.error("Failed to sync schema", error);
      return c.json({ error: "Failed to sync schema" }, 500);
    }
  });

  // Sync augmentations from Platform
  app.post("/sync/augmentations", async (c) => {
    const connectionId = c.get("connectionId");
    const logger = c.get("logger");
    const body = c.get("parsedBody");

    if (!deps.writeAugmentations) {
      logger.error("writeAugmentations not configured");
      return c.json({ error: "Augmentations sync not supported" }, 501);
    }

    try {
      const payload = unifiedAugmentationSchema.parse(body);
      logger.info("Syncing augmentations", {
        relations: payload.relations?.length ?? 0,
        computedColumns: payload.computedColumns?.length ?? 0,
        columnConversions: payload.columnConversions?.length ?? 0,
      });

      await deps.writeAugmentations(connectionId, {
        ...payload,
        updatedAt: payload.updatedAt ?? new Date().toISOString(),
      });
      deps.clearCache?.(connectionId);
      logger.info("Augmentations synced successfully");
      return c.json({ ok: true });
    } catch (error) {
      if (error instanceof ZodError) {
        logger.warn("Invalid augmentations payload", { issues: error.issues });
        return c.json({ error: "Invalid payload" }, 400);
      }
      logger.error("Failed to sync augmentations", error);
      return c.json({ error: "Failed to sync augmentations" }, 500);
    }
  });

  // Clear all caches for a connection
  app.post("/cache/clear", async (c) => {
    const connectionId = c.get("connectionId");
    const logger = c.get("logger");

    if (!deps.clearAllCaches) {
      logger.error("clearAllCaches not configured");
      return c.json({ error: "Cache clear not supported" }, 501);
    }

    try {
      logger.info("Clearing all caches");
      deps.clearAllCaches(connectionId);
      logger.info("All caches cleared successfully");
      return c.json({ ok: true });
    } catch (error) {
      logger.error("Failed to clear caches", error);
      return c.json({ error: "Failed to clear caches" }, 500);
    }
  });

  // Execute query
  app.post("/", async (c) => {
    const connectionId = c.get("connectionId");
    const logger = c.get("logger");
    const body = c.get("parsedBody");
    const startTime = Date.now();

    try {
      if (deps.validateAugmentationsHash) {
        const expectedHash = c.req.header("inconvo-augmentations-hash") ?? undefined;
        try {
          await deps.validateAugmentationsHash(connectionId, expectedHash);
        } catch (error) {
          logger.error("Augmentations sync failed", error);
          return c.json(
            {
              error: "Augmentations sync failed",
              details: error instanceof Error ? error.message : String(error),
            },
            503,
          );
        }
      }

      const parsedQuery = QuerySchema.parse(body);
      const { operation } = parsedQuery;

      logger.info("Executing query", { operation, table: parsedQuery.table });

      const { db, schema, dialect } = await deps.loadSchemaAndAugmentations(connectionId);
      const ctx: OperationContext = { schema, dialect };

      let response;
      switch (operation) {
        case "aggregate":
          response = await aggregate(db, parsedQuery, ctx);
          break;
        case "count":
          response = await count(db, parsedQuery, ctx);
          break;
        case "countRelations":
          response = await countRelations(db, parsedQuery, ctx);
          break;
        case "findDistinct":
          response = await findDistinct(db, parsedQuery, ctx);
          break;
        case "findDistinctByEditDistance":
          response = await findDistinctByEditDistance(db, parsedQuery, ctx);
          break;
        case "findMany":
          response = await findMany(db, parsedQuery, ctx);
          break;
        case "aggregateGroups":
          response = await aggregateGroups(db, parsedQuery, ctx);
          break;
        case "groupBy":
          response = await groupBy(db, parsedQuery, ctx);
          break;
        default:
          logger.warn("Invalid operation requested", { operation });
          return c.json({ error: "Invalid operation" }, 400);
      }

      const duration = Date.now() - startTime;
      logger.timing("Query completed", duration, {
        operation,
        table: parsedQuery.table,
      });

      return c.body(safeJsonStringify(response), 200, {
        "Content-Type": "application/json",
      });
    } catch (error) {
      const duration = Date.now() - startTime;

      if (error instanceof ZodError) {
        logger.warn("Query validation failed", {
          durationMs: duration,
          issues: error.issues,
        });
        return c.json({ error: error }, 400);
      }

      if (error instanceof QueryExecutionError) {
        logger.error("Query execution failed with details", error, {
          durationMs: duration,
          sql: error.details.sql,
          operation: error.details.operation,
        });
        return c.body(
          safeJsonStringify({ error: error.details }),
          500,
          { "Content-Type": "application/json" },
        );
      }

      logger.error("Query execution failed", error, { durationMs: duration });
      return c.json({ error: "Failed to execute query" }, 500);
    }
  });

  return app;
}

export { createHmacMiddleware } from "./middleware/hmac";
export type { SchemaTable, SchemaResponse } from "../types/types";
export type { DatabaseDialect, OperationContext } from "../operations/types";

// Export schema introspection utilities (doesn't depend on env)
export { buildSchemaFromDb } from "../util/buildSchemaFromDb";
export type {
  IntrospectionConfig,
  IntrospectionDialect,
  IntrospectionLogger,
} from "../util/buildSchemaFromDb";

// Utility export for Lambda hash parity checks.
export { computeAugmentationsHash } from "../util/schemaAugmentationStore";
