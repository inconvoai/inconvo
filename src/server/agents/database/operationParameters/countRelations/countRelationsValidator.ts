import { z } from "zod";
import type { CountRelationsQuery } from "~/server/userDatabaseConnector/types";
import { stringArrayToZodEnum } from "~/server/agents/utils/zodHelpers";

export interface CountRelationsRelationOption {
  name: string;
  targetColumns: string[]; // unqualified column names in the related table
}

export interface CountRelationsValidatorContext {
  baseColumns: string[]; // columns of the base table (unqualified)
  relationOptions: CountRelationsRelationOption[]; // outward list relations only
}

export interface CountRelationsValidResult {
  status: "valid";
  result: CountRelationsQuery["operationParameters"];
}

export interface CountRelationsInvalidResultIssue {
  path: string;
  message: string;
  code: string;
}

export interface CountRelationsInvalidResult {
  status: "invalid";
  issues: CountRelationsInvalidResultIssue[];
}

export type CountRelationsValidationResult =
  | CountRelationsValidResult
  | CountRelationsInvalidResult;

export function buildCountRelationsZodSchema(
  ctx: CountRelationsValidatorContext
) {
  const baseColEnum = stringArrayToZodEnum(ctx.baseColumns);

  // Build per relation literal object schemas so tool guidance is strict
  const relationLiteralSchemas = ctx.relationOptions.map((rel) => {
    const distinctEnum = rel.targetColumns.length
      ? (stringArrayToZodEnum(rel.targetColumns) as z.ZodType<string>)
      : z.never();
    return z.object({
      name: z.literal(rel.name),
      distinct: rel.targetColumns.length
        ? distinctEnum.nullable()
        : z.null().describe("No distinct columns available"),
    });
  });

  if (ctx.relationOptions.length === 0) {
    // No relations available: build a schema that will always fail relationsToCount min(1)
    return z.object({
      columns: z.array(baseColEnum).min(1),
      relationsToCount: z.array(z.never()).min(1),
      orderBy: z
        .object({
          relation: z.string(),
          direction: z.enum(["asc", "desc"]),
        })
        .nullable(),
      limit: z.number().int().positive().max(1000),
    });
  }

  // ctx.relationOptions length guaranteed > 0 past early return above
  const relationNameEnum = stringArrayToZodEnum(
    ctx.relationOptions.map((r) => r.name)
  );

  return z.object({
    columns: z
      .array(baseColEnum)
      .min(1, "Select at least one base table column to include"),
    relationsToCount: z
      .array(
        z.union(
          relationLiteralSchemas as unknown as [
            z.ZodTypeAny,
            z.ZodTypeAny,
            ...z.ZodTypeAny[]
          ]
        )
      )
      .min(1, "Select at least one relation to count"),
    orderBy: z
      .object({
        relation: relationNameEnum,
        direction: z.enum(["asc", "desc"]),
      })
      .nullable(),
    limit: z.number().int().positive().max(1000),
  });
}

export function validateCountRelationsCandidate(
  candidateOperationParameters: unknown,
  ctx: CountRelationsValidatorContext
): CountRelationsValidationResult {
  const schema = buildCountRelationsZodSchema(ctx);
  const parsed = schema.safeParse(candidateOperationParameters);
  if (!parsed.success) {
    return {
      status: "invalid",
      issues: parsed.error.issues.map((i) => ({
        path: i.path.join(".") || "<root>",
        message: i.message,
        code: i.code,
      })),
    };
  }

  const data = parsed.data;
  const issues: CountRelationsInvalidResultIssue[] = [];

  // Type the relationsToCount properly to avoid unsafe access
  const relationsToCount = data.relationsToCount as Array<{
    name: string;
    distinct: string | null;
  }>;

  // Ensure unique relation names
  const relationNames = relationsToCount.map((r) => r.name);
  const relationSet = new Set(relationNames);
  if (relationSet.size !== relationNames.length) {
    issues.push({
      path: "relationsToCount",
      message: "Duplicate relation names are not allowed",
      code: "duplicate_relations",
    });
  }

  // Verify distinct columns are valid for each relation
  const relMap = Object.fromEntries(
    ctx.relationOptions.map((r) => [r.name, r.targetColumns])
  );
  relationsToCount.forEach((rel, idx) => {
    if (rel.distinct !== null) {
      const allowed = relMap[rel.name] ?? [];
      if (!allowed.includes(rel.distinct)) {
        issues.push({
          path: `relationsToCount.${idx}.distinct`,
          message: `Distinct column ${rel.distinct} not found in relation ${rel.name}`,
          code: "invalid_distinct",
        });
      }
    }
  });

  // orderBy relation must be among listed relationsToCount
  if (data.orderBy && !relationSet.has(data.orderBy.relation)) {
    issues.push({
      path: "orderBy.relation",
      message: "orderBy.relation must be one of relationsToCount names",
      code: "orderBy_not_counted",
    });
  }

  if (issues.length) {
    return { status: "invalid", issues };
  }

  const result: CountRelationsQuery["operationParameters"] = {
    columns: data.columns,
    relationsToCount: relationsToCount.map((r) => ({
      name: r.name,
      distinct: r.distinct ?? null,
    })),
    orderBy: data.orderBy ?? null,
    limit: data.limit,
  };

  return { status: "valid", result };
}
