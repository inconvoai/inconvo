import express from "express";
import dotenv from "dotenv";
import cors from "cors";
import path from "path";
import { fileURLToPath } from "url";
import { inconvo } from "@ten-dev/inconvo/express";
import pino from "pino";

const logger = pino({
  level: process.env.LOG_LEVEL || "info",
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

dotenv.config();

const app = express();
const port = 3006;

app.use(express.json());
app.use(express.urlencoded({ extended: true }));

app.use("/inconvo", await inconvo());

app.get("/", (req, res) => {
  res.send("OK - Inconvo server running");
});

app.use(cors());
const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);
app.use(express.static(path.join(__dirname, "public")));

app.listen(port, () => {
  logger.info(`QueryEngine is running on port ${port}`);
});
