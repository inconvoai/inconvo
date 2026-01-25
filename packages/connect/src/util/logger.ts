import pino, { type LevelWithSilent } from "pino";

const isTestEnv =
  process.env.NODE_ENV === "test" || Boolean(process.env.JEST_WORKER_ID);
const isLambda = Boolean(process.env.AWS_LAMBDA_FUNCTION_NAME);
const isProd = process.env.NODE_ENV === "production";

const envLogLevel = process.env.LOG_LEVEL
  ? (process.env.LOG_LEVEL.toLowerCase() as LevelWithSilent)
  : undefined;
const resolvedLevel: LevelWithSilent =
  envLogLevel ?? (isTestEnv ? "silent" : "debug");

// Use pino-pretty only in local development (not in Lambda or production)
// pino-pretty spawns a worker thread which doesn't work in bundled Lambda code
const usePrettyPrint = !isLambda && !isProd && !isTestEnv;

export const logger = pino({
  level: resolvedLevel,
  serializers: {
    error: pino.stdSerializers.err,
  },
  ...(usePrettyPrint
    ? {
        transport: {
          target: "pino-pretty",
          options: {
            translateTime: "HH:MM:ss",
            ignore: "pid,hostname,time,level",
            messageFormat: "[{level}]:{msg}",
          },
        },
      }
    : {}),
  formatters: {
    level: (label: string) => {
      return { level: label.toUpperCase() };
    },
  },
});
