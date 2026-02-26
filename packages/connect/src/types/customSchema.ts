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

export const columnRenameAugmentationItemSchema = z
  .object({
    table: z.string(),
    dbName: z.string(),
    semanticName: z.string(),
  })
  .strict();

const virtualTableDialectSchema = z.enum([
  "postgresql",
  "redshift",
  "mysql",
  "mssql",
  "bigquery",
]);

const virtualTableAccessSchema = z.enum(["QUERYABLE", "JOINABLE", "OFF"]);
const virtualTableRelationStatusSchema = z.enum(["VALID", "BROKEN"]);

export const virtualTableColumnAugmentationItemSchema = z
  .object({
    sourceName: z.string(),
    name: z.string(),
    type: z.string(),
    nullable: z.boolean().nullable().optional(),
    selected: z.boolean().optional(),
    unit: z.string().nullable().optional(),
    notes: z.string().nullable().optional(),
  })
  .strict();

export const virtualTableRelationAugmentationItemSchema = z
  .object({
    name: z.string(),
    targetTable: z.string(),
    isList: z.boolean(),
    sourceColumns: z.array(z.string()).min(1),
    targetColumns: z.array(z.string()).min(1),
    selected: z.boolean().optional(),
    status: virtualTableRelationStatusSchema.optional(),
    errorTag: z.string().nullable().optional(),
  })
  .strict()
  .superRefine((value, ctx) => {
    if (value.sourceColumns.length !== value.targetColumns.length) {
      ctx.addIssue({
        code: z.ZodIssueCode.custom,
        message: "sourceColumns and targetColumns must have the same length",
        path: ["sourceColumns"],
      });
    }
  });

export const virtualTableConditionAugmentationSchema = z
  .object({
    column: z.string(),
    userContextFieldKey: z.string(),
  })
  .strict();

export const virtualTableAccessPolicyAugmentationSchema = z
  .object({
    userContextFieldKey: z.string(),
  })
  .strict();

export const virtualTableAugmentationItemSchema = z
  .object({
    name: z.string(),
    dialect: virtualTableDialectSchema,
    sql: z.string(),
    selected: z.boolean().optional(),
    access: virtualTableAccessSchema.optional(),
    context: z.string().nullable().optional(),
    columns: z.array(virtualTableColumnAugmentationItemSchema),
    relations: z.array(virtualTableRelationAugmentationItemSchema),
    condition: virtualTableConditionAugmentationSchema.nullable().optional(),
    accessPolicy: virtualTableAccessPolicyAugmentationSchema.nullable().optional(),
  })
  .strict();

// Unified augmentation schema - combines all three augmentation types
// Hash is optional for storage (legacy data may not have it)
export const unifiedAugmentationSchema = z
  .object({
    updatedAt: z.iso.datetime().optional(),
    hash: z.string().optional(),
    relations: z.array(manualRelationSchema),
    computedColumns: z.array(computedColumnAugmentationItemSchema),
    columnConversions: z.array(columnConversionAugmentationItemSchema),
    columnRenames: z.array(columnRenameAugmentationItemSchema).optional().default([]),
    virtualTables: z.array(virtualTableAugmentationItemSchema).optional().default([]),
  })
  .strict();

export type UnifiedAugmentation = z.infer<typeof unifiedAugmentationSchema>;
