import express from "express";
import type { Request, Response, NextFunction } from "express";
import dotenv from "dotenv";
dotenv.config();

import cors from "cors";
import path from "path";
import { fileURLToPath } from "url";
import { inconvo } from "@repo/connect/express";
import pino from "pino";
import { env } from "./env";
import { validateAndSyncAugmentations } from "./augmentationSyncService";

const logger = pino({
  level: process.env.LOG_LEVEL ?? "info",
  transport: {
    target: "pino-pretty",
    options: {
      translateTime: "HH:MM:ss",
      ignore: "pid,hostname,time,level",
      messageFormat: "[{level}]:{msg}",
    },
  },
  formatters: {
    level: (label) => {
      return { level: label.toUpperCase() };
    },
  },
});

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

const PORT = env.PORT;

/**
 * Middleware to validate augmentations hash and sync from platform if needed.
 * Only applies to POST requests (query operations).
 */
async function augmentationsSyncMiddleware(
  req: Request,
  res: Response,
  next: NextFunction,
) {
  // Only check hash on POST requests (queries)
  if (req.method !== "POST") {
    return next();
  }

  const incomingHash = req.headers["inconvo-augmentations-hash"] as
    | string
    | undefined;

  if (!incomingHash) {
    console.log("No augmentations hash provided, skipping sync");
    return next();
  }

  const syncResult = await validateAndSyncAugmentations(incomingHash);
  if (syncResult.error) {
    logger.error(
      { error: syncResult.error },
      "Augmentations sync failed, blocking request",
    );
    return res.status(503).json({
      error: "Augmentations sync failed",
      details: syncResult.error,
    });
  }

  next();
}

async function main() {
  const app = express();

  app.use(cors());
  app.use(express.json());
  app.use(express.urlencoded({ extended: true }));

  // Add augmentations sync middleware before the inconvo router
  app.use("/inconvo", augmentationsSyncMiddleware, await inconvo());

  app.get("/", (_req: Request, res: Response) => {
    res.send("OK - Inconvo Connect server running");
  });

  app.use(express.static(path.join(__dirname, "public")));

  app.listen(PORT, () => {
    logger.info(`Connect server running on port ${PORT}`);
  });
}

main().catch((err) => {
  logger.error(err, "Failed to start server");
  process.exit(1);
});
