import { Router } from "express";
import type { Request, Response } from "express";
import { authenticated } from "./middlewares";
import { QuerySchema } from "../types/querySchema";
import { ZodError } from "zod";
import {
  clearSchemaCache,
  getCachedSchema,
  preloadSchema,
} from "../util/schemaCache";
import { clearAugmentedSchemaCache, getAugmentedSchema } from "../util/augmentedSchemaCache";
import { getDb } from "../dbConnection";
import packageJson from "../../package.json" with { type: "json" };
import { logger } from "../util/logger";
import { aggregate } from "../operations/aggregate";
import { aggregateGroups } from "../operations/aggregateGroups";
import { findMany } from "../operations/findMany";
import { count } from "../operations/count";
import { countRelations } from "../operations/countRelations";
import { groupBy } from "../operations/groupBy";
import { findDistinct } from "../operations/findDistinct";
import { findDistinctByEditDistance } from "../operations/findDistinctByEditDistance";
import { writeUnifiedAugmentation } from "../util/schemaAugmentationStore";
import { unifiedAugmentationSchema } from "../types/customSchema";
import { checkDatabaseHealth } from "../util/healthCheck";
import { logDatabaseHealthCheckHint } from "../util/databaseDiagnostics";
import { env } from "../env";
import type { OperationContext } from "../operations/types";
import { QueryExecutionError } from "../util/queryErrors";

function safeJsonStringify(value: unknown): string {
  return JSON.stringify(value, (key, val) => {
    if (typeof val === "bigint") {
      return val.toString();
    }
    return val;
  });
}

export async function inconvo(): Promise<Router> {
  const router = Router();

  router.get("/healthz", async (_req: Request, res: Response) => {
    const startedAt = Date.now();
    try {
      await checkDatabaseHealth();
      res
        .status(200)
        .setHeader("Content-Type", "application/json")
        .send(
          safeJsonStringify({
            ok: true,
            duration: Date.now() - startedAt,
          }),
        );
    } catch (error) {
      logger.error({ error }, "GET /healthz - health check failed");
      logDatabaseHealthCheckHint(error);
      res
        .status(503)
        .setHeader("Content-Type", "application/json")
        .send(
          safeJsonStringify({
            ok: false,
            error: "Database health check failed",
          }),
        );
    }
  });

  router.use(authenticated);

  // Preload schema on startup to cache it
  try {
    logger.info("Running database health check");
    await checkDatabaseHealth();
    logger.info("Database health check succeeded");
  } catch (error) {
    logger.error({ error }, "Database health check failed - aborting startup");
    logDatabaseHealthCheckHint(error);
    throw error;
  }

  try {
    logger.info("Starting to pull schema");
    await preloadSchema();
    logger.info("Schema preloaded successfully");
  } catch (error) {
    logger.warn({ error }, "Failed to preload schema - will load on first use");
    // Continue anyway - schema will be loaded on first use
  }

  router.get("/", async (req: Request, res: Response) => {
    try {
      clearSchemaCache();
      clearAugmentedSchemaCache();
      const schema = await getCachedSchema();
      const sanitizedTables = schema.tables.map(
        ({
          computedColumns: _computedColumns,
          columnConversions: _columnConversions,
          ...table
        }) => ({
          ...table,
          // Strip internal STRUCT metadata from columns (BigQuery-specific)
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
      const publicSchema = {
        tables: sanitizedTables,
        databaseSchemas: schema.databaseSchemas,
      };
      res.setHeader("Content-Type", "application/json");
      res.send(safeJsonStringify(publicSchema));
    } catch (error) {
      logger.error({ error }, "GET / - Failed to fetch schema");
      res.status(500).setHeader("Content-Type", "application/json");
      res.send(safeJsonStringify({ error: "Failed to fetch schema" }));
    }
  });

  router.get("/version", (req: Request, res: Response) => {
    res.setHeader("Content-Type", "application/json");
    res.send(safeJsonStringify({ version: packageJson.version }));
  });

  // Unified sync endpoint for all augmentations (push from platform)
  router.post("/sync/augmentations", async (req: Request, res: Response) => {
    try {
      const payload = unifiedAugmentationSchema.parse(req.body);
      await writeUnifiedAugmentation({
        ...payload,
        updatedAt: payload.updatedAt ?? new Date().toISOString(),
      });
      clearAugmentedSchemaCache();
      res
        .status(200)
        .setHeader("Content-Type", "application/json")
        .send(safeJsonStringify({ ok: true }));
    } catch (error) {
      if (error instanceof ZodError) {
        logger.warn(
          { issues: error.issues },
          "POST /sync/augmentations - invalid payload",
        );
        return res
          .status(400)
          .setHeader("Content-Type", "application/json")
          .send(safeJsonStringify({ error: "Invalid payload" }));
      }
      logger.error({ error }, "POST /sync/augmentations - failed");
      res
        .status(500)
        .setHeader("Content-Type", "application/json")
        .send(safeJsonStringify({ error: "Failed to persist augmentations" }));
    }
  });

  router.post("/", async (req: Request, res: Response) => {
    const startTime = Date.now();

    try {
      const parsedQuery = QuerySchema.parse(req.body);
      const { operation } = parsedQuery;
      logger.info({ operation }, "POST / - Executing operation");
      const db = await getDb();
      const schema = await getAugmentedSchema();
      const ctx: OperationContext = { schema, dialect: env.DATABASE_DIALECT };

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
          throw new Error("Invalid inconvo operation");
      }

      const duration = Date.now() - startTime;
      logger.info(
        { duration },
        `POST / - Operation completed in ${duration}ms`,
      );
      res.setHeader("Content-Type", "application/json");
      return res.send(safeJsonStringify(response));
    } catch (error) {
      const duration = Date.now() - startTime;
      if (error instanceof ZodError) {
        logger.error(
          { duration, issues: error.issues },
          `POST / - Validation error after ${duration}ms`,
        );
        return res
          .status(400)
          .setHeader("Content-Type", "application/json")
          .send(safeJsonStringify({ error: error }));
      }
      if (error instanceof QueryExecutionError) {
        logger.error(
          {
            duration,
            error: error.details,
            sql: error.details.sql,
            operation: error.details.operation,
          },
          `POST / - Query execution failed with details after ${duration}ms`,
        );
        return res
          .status(500)
          .setHeader("Content-Type", "application/json")
          .send(safeJsonStringify({ error: error.details }));
      }
      logger.error(
        { error, duration },
        `POST / - Operation failed after ${duration}ms`,
      );
      res
        .status(500)
        .setHeader("Content-Type", "application/json")
        .send(safeJsonStringify({ error: "Failed to execute query" }));
    }
  });

  return router;
}
