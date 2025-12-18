import express from "express";
import type { Request, Response } from "express";
import dotenv from "dotenv";
import cors from "cors";
import path from "path";
import { fileURLToPath } from "url";
import { inconvo } from "@ten-dev/inconvo/express";
import pino from "pino";

dotenv.config();

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

const PORT = process.env.PORT ?? 3006;

async function main() {
  const app = express();

  app.use(cors());
  app.use(express.json());
  app.use(express.urlencoded({ extended: true }));

  app.use("/inconvo", await inconvo());

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
