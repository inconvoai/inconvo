import { z } from "zod";
import {
  joinDescriptorSchema,
  type CountRelationsQuery,
  type JoinPathHop,
} from "~/server/userDatabaseConnector/types";
import { stringArrayToZodEnum } from "~/server/agents/utils/zodHelpers";

const countRelationsJoinSchema = joinDescriptorSchema
  .extend({
    name: z.string().min(1),
  })
  .strip();

type CountRelationsJoinInput = z.infer<typeof countRelationsJoinSchema>;

export interface CountRelationsRelationOption {
  name: string;
  table: string;
  path: JoinPathHop[];
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
          name: z.string(),
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

  const joinDescription =
    ctx.relationOptions.length > 0
      ? [
          "Array of join descriptors. Each relation must appear here with name matching the relation.",
          "Available joins:",
          ...ctx.relationOptions.map(
            (option) =>
              `${option.name} (${option.table}) path=${formatJoinPath(
                option.path
              )}`
          ),
        ].join("\n")
      : "Array of join descriptors (no relations available).";

  return z.object({
    columns: z
      .array(baseColEnum)
      .min(1, "Select at least one base table column to include"),
    joins: z
      .array(countRelationsJoinSchema)
      .nullable()
      .optional()
      .describe(joinDescription),
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
        name: relationNameEnum,
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

  const joinsInput =
    "joins" in data
      ? (data as { joins?: CountRelationsJoinInput[] | null }).joins ?? null
      : null;

  const validatedJoins = validateJoins(
    joinsInput,
    ctx,
    issues
  );

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

  relationsToCount.forEach((rel, idx) => {
    const hasJoin = validatedJoins?.some((join) => join.name === rel.name);
    if (!hasJoin) {
      issues.push({
        path: `relationsToCount.${idx}.name`,
        message: `Relation ${rel.name} must have a matching join descriptor in joins array.`,
        code: "missing_join_descriptor",
      });
    }
  });

  // Disallow stray joins not tied to relation options
  if (validatedJoins) {
    validatedJoins.forEach((join, index) => {
      const relationAllowed = ctx.relationOptions.some(
        (option) => option.name === join.name
      );
      if (!relationAllowed) {
        issues.push({
          path: `joins.${index}.name`,
          message: `Join alias ${join.name} does not match any countable relation.`,
          code: "unknown_join_alias",
        });
      }
    });
  }

  // orderBy relation must be among listed relationsToCount
  if (data.orderBy && !relationSet.has(data.orderBy.name)) {
    issues.push({
      path: "orderBy.name",
      message: "orderBy.name must be one of relationsToCount names",
      code: "orderBy_not_counted",
    });
  }

  if (issues.length) {
    return { status: "invalid", issues };
  }

  const result: CountRelationsQuery["operationParameters"] = {
    columns: data.columns,
    joins: validatedJoins ?? null,
    relationsToCount: relationsToCount.map((r) => ({
      name: r.name,
      distinct: r.distinct ?? null,
    })),
    orderBy: data.orderBy ?? null,
    limit: data.limit,
  };

  return { status: "valid", result };
}

function validateJoins(
  joins: CountRelationsJoinInput[] | null | undefined,
  ctx: CountRelationsValidatorContext,
  issues: CountRelationsInvalidResultIssue[]
): CountRelationsQuery["operationParameters"]["joins"] | undefined {
  if (!joins || joins.length === 0) {
    return undefined;
  }

  const optionsByName = new Map(
    ctx.relationOptions.map((option) => [option.name, option])
  );
  const seenAliases = new Set<string>();

  const validated = joins
    .map((join, index) => {
      const alias = join.name;
      if (seenAliases.has(alias)) {
        issues.push({
          path: `joins.${index}.name`,
          message: `Join alias ${alias} appears more than once.`,
          code: "duplicate_join_alias",
        });
        return null;
      }

      const option = optionsByName.get(alias);
      if (!option) {
        issues.push({
          path: `joins.${index}.name`,
          message: `Join alias ${alias} does not correspond to a known relation.`,
          code: "unknown_join_alias",
        });
        return null;
      }

      const candidateKey = joinPathKey(join.path);
      const expectedKey = joinPathKey(option.path);
      if (candidateKey !== expectedKey) {
        issues.push({
          path: `joins.${index}.path`,
          message: `Join path for ${alias} does not match the relation's columns.`,
          code: "invalid_join_path",
        });
      }

      if (join.table !== option.table) {
        issues.push({
          path: `joins.${index}.table`,
          message: `Join table must be ${option.table} for relation ${alias}.`,
          code: "invalid_join_table",
        });
      }

      seenAliases.add(alias);

      return {
        name: option.name,
        table: option.table,
        path: option.path,
        joinType: join.joinType,
      } satisfies NonNullable<
        CountRelationsQuery["operationParameters"]["joins"]
      >[number];
    })
    .filter((entry): entry is NonNullable<typeof entry> => entry !== null);

  return validated.length > 0 ? validated : undefined;
}

function joinPathKey(path: JoinPathHop[]) {
  return path
    .map((hop) => `${hop.source.join(",")}=>${hop.target.join(",")}`)
    .join("|");
}

function formatJoinPath(path: JoinPathHop[]) {
  return path
    .map(
      (hop) => `[${hop.source.join(", ")}] -> [${hop.target.join(", ")}]`
    )
    .join(" | ");
}
