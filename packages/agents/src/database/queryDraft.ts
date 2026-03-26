import { z } from "zod";

export const supportedDatabaseRetrieverOperations = [
  "findMany",
  "findDistinct",
  "count",
  "countRelations",
  "aggregate",
  "aggregateGroups",
  "groupBy",
] as const;

export const databaseRetrieverOperationSchema = z.enum(
  supportedDatabaseRetrieverOperations,
);

export const databaseRetrieverQueryDraftSchema = z
  .object({
    table: z.string().min(1),
    operation: databaseRetrieverOperationSchema,
    operationParameters: z.object({}).passthrough(),
    questionConditions: z
      .union([z.object({}).passthrough(), z.null()])
      .default(null),
  })
  .strict();

export type SupportedDatabaseRetrieverOperation = z.infer<
  typeof databaseRetrieverOperationSchema
>;

export type DatabaseRetrieverQueryDraft = z.infer<
  typeof databaseRetrieverQueryDraftSchema
>;
