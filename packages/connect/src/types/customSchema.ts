import { z } from "zod";
import { SQLCastExpressionAstSchema } from "./querySchema";

export const manualRelationSchema = z
  .object({
    name: z.string(),
    relationId: z.string().optional(),
    isList: z.boolean(),
    sourceTable: z.string(),
    targetTable: z.string(),
    sourceColumns: z.array(z.string()).min(1, "sourceColumns required"),
    targetColumns: z.array(z.string()).min(1, "targetColumns required"),
    selected: z.boolean().optional(),
    status: z.enum(["VALID", "BROKEN"]).optional(),
    errorTag: z.string().nullable().optional(),
  })
  .strict();

export type ManualRelationPayload = z.infer<typeof manualRelationSchema>;

export const customRelationsAugmentationSchema = z
  .object({
    updatedAt: z.iso.datetime().optional(),
    relations: z.array(manualRelationSchema),
  })
  .strict();

export type CustomRelationsAugmentation = z.infer<
  typeof customRelationsAugmentationSchema
>;

export const computedColumnAugmentationItemSchema = z
  .object({
    name: z.string(),
    table: z.string(),
    ast: z.unknown(),
    type: z.string().optional(),
    unit: z.string().nullable().optional(),
    notes: z.string().nullable().optional(),
    selected: z.boolean().optional(),
  })
  .strict();

export const computedColumnsAugmentationSchema = z
  .object({
    updatedAt: z.iso.datetime().optional(),
    computedColumns: z.array(computedColumnAugmentationItemSchema),
  })
  .strict();

export type ComputedColumnsAugmentation = z.infer<
  typeof computedColumnsAugmentationSchema
>;

export const columnConversionAugmentationItemSchema = z
  .object({
    column: z.string(),
    table: z.string(),
    ast: SQLCastExpressionAstSchema,
    type: z.string().optional(),
    selected: z.boolean().optional(),
  })
  .strict();

export const columnConversionsAugmentationSchema = z
  .object({
    updatedAt: z.iso.datetime().optional(),
    columnConversions: z.array(columnConversionAugmentationItemSchema),
  })
  .strict();

export type ColumnConversionsAugmentation = z.infer<
  typeof columnConversionsAugmentationSchema
>;

// Unified augmentation schema - combines all three augmentation types
// Hash is optional for storage (legacy data may not have it)
export const unifiedAugmentationSchema = z
  .object({
    updatedAt: z.iso.datetime().optional(),
    hash: z.string().optional(),
    relations: z.array(manualRelationSchema),
    computedColumns: z.array(computedColumnAugmentationItemSchema),
    columnConversions: z.array(columnConversionAugmentationItemSchema),
  })
  .strict();

export type UnifiedAugmentation = z.infer<typeof unifiedAugmentationSchema>;
