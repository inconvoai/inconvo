import { z } from "zod";

const envSchema = z.object({
  PORT: z.string().default("3006"),
  LOG_LEVEL: z.string().default("info"),
  INCONVO_PLATFORM_URL: z.url(),
  INCONVO_API_KEY: z.string().min(1),
  INCONVO_AGENT_ID: z.string().min(1),
});

export const env = envSchema.parse(process.env);
