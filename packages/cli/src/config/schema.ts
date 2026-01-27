import { z } from "zod";

export const databaseDialectSchema = z.enum(["postgresql", "mysql", "mssql"]);
export type DatabaseDialect = z.infer<typeof databaseDialectSchema>;

export const configSchema = z.object({
  version: z.literal(1),
  database: z.object({
    dialect: databaseDialectSchema,
    url: z.string().min(1),
    schema: z.string().optional(),
  }),
  apiKeys: z.object({
    openai: z.string().startsWith("sk-"),
    langchain: z.string().min(1),
  }),
  secrets: z.object({
    internalApiKey: z.string().min(1),
    inconvoSecretKey: z.string().regex(/^local-dev-[a-f0-9-]{36}$/),
  }),
});

export type InconvoConfig = z.infer<typeof configSchema>;
