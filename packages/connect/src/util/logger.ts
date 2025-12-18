import pino, { type LevelWithSilent } from "pino";

const isTestEnv =
  process.env.NODE_ENV === "test" || Boolean(process.env.JEST_WORKER_ID);
const envLogLevel = process.env.LOG_LEVEL
  ? (process.env.LOG_LEVEL.toLowerCase() as LevelWithSilent)
  : undefined;
const resolvedLevel: LevelWithSilent =
  envLogLevel ?? (isTestEnv ? "silent" : "debug");

export const logger = pino({
  level: resolvedLevel,
  transport: {
    target: "pino-pretty",
    options: {
      translateTime: "HH:MM:ss",
      ignore: "pid,hostname,time,level",
      messageFormat: "[{level}]:{msg}",
    },
  },
  formatters: {
    level: (label: string) => {
      return { level: label.toUpperCase() };
    },
  },
});
