import { z } from "zod";
import { stringArrayToZodEnum } from "../../../utils/zodHelpers";
import {
  joinDescriptorSchema,
  type FindManyQuery,
  type JoinPathHop,
} from "@repo/types";

export interface FindManyJoinOption {
  name: string;
  table: string;
  path: JoinPathHop[];
  selectableColumns: string[];
}

export interface FindManyValidatorContext {
  baseTable: string;
  selectableTableColumns: Record<string, string[]>; // alias -> columns (including computed)
  baseColumns: string[];
  baseComputedColumns: string[];
  joinOptions: FindManyJoinOption[];
}

export interface FindManyInvalidResultIssue {
  path: string;
  message: string;
  code: string;
}

export interface FindManyInvalidResult {
  status: "invalid";
  issues: FindManyInvalidResultIssue[];
}

export type FindManyValidationResult =
  | { status: "valid"; result: FindManyQuery["operationParameters"] }
  | FindManyInvalidResult;

export function buildFindManyZodSchema(ctx: FindManyValidatorContext) {
  const selectValidator = z.object(
    Object.entries(ctx.selectableTableColumns).reduce(
      (
        acc: Record<
          string,
          z.ZodOptional<z.ZodNullable<z.ZodArray<z.ZodType<string>>>>
        >,
        [alias, columns],
      ) => {
        if (!columns?.length) return acc;
        acc[alias] = z
          .array(stringArrayToZodEnum(columns))
          .nullable()
          .optional();
        return acc;
      },
      {} as Record<
        string,
        z.ZodOptional<z.ZodNullable<z.ZodArray<z.ZodType<string>>>>
      >,
    ),
  );

  return z.object({
    select: selectValidator
      .describe(
        "Keys are table aliases (e.g. users.orders). Only include paths you need.",
      )
      .refine(
        (obj) =>
          Object.values(obj).some(
            (value) => Array.isArray(value) && value.length > 0,
          ),
        "Select must include at least one column.",
      ),
    joins: z.array(joinDescriptorSchema).nullable().optional(),
    orderBy: z
      .object({
        direction: z.enum(["asc", "desc"]),
        column: stringArrayToZodEnum([
          ...ctx.baseColumns,
          ...ctx.baseComputedColumns,
        ]).describe("Ordering column (must come from base table)."),
      })
      .nullable()
      .describe("Optional ordering for base table only"),
    limit: z
      .number()
      .int()
      .positive()
      .max(1000)
      .describe("Number of records to return (<= 1000)"),
  });
}

export function validateFindManyCandidate(
  candidate: unknown,
  ctx: FindManyValidatorContext,
): FindManyValidationResult {
  const schema = buildFindManyZodSchema(ctx);
  const parsed = schema.safeParse(candidate);

  if (!parsed.success) {
    const issues: FindManyInvalidResultIssue[] = parsed.error.issues.map(
      (issue) => ({
        path: issue.path.join(".") || "<root>",
        message: issue.message,
        code: issue.code,
      }),
    );
    return { status: "invalid", issues };
  }

  const issues: FindManyInvalidResultIssue[] = [];

  const filteredSelect = Object.entries(parsed.data.select).reduce(
    (acc: Record<string, string[]>, [alias, columns]) => {
      if (Array.isArray(columns) && columns.length > 0) {
        acc[alias] = columns;
      }
      return acc;
    },
    {},
  );

  const validatedJoins = validateJoins(parsed.data.joins ?? [], ctx, issues);

  const joinAliases = new Set(
    (validatedJoins ?? []).map((join) => join.name ?? join.table),
  );
  Object.keys(filteredSelect).forEach((alias) => {
    if (alias === ctx.baseTable) {
      return;
    }
    if (!joinAliases.has(alias)) {
      issues.push({
        path: `select.${alias}`,
        message: `Join alias ${alias} must be defined in the joins array.`,
        code: "missing_join_for_select",
      });
    }
  });

  if (issues.length > 0) {
    return { status: "invalid", issues };
  }

  return {
    status: "valid",
    result: {
      select: filteredSelect,
      joins: validatedJoins,
      orderBy: parsed.data.orderBy ?? null,
      limit: parsed.data.limit,
    },
  };
}

function validateJoins(
  joins: z.infer<typeof joinDescriptorSchema>[] | null | undefined,
  ctx: FindManyValidatorContext,
  issues: FindManyInvalidResultIssue[],
): FindManyQuery["operationParameters"]["joins"] {
  if (!joins || joins.length === 0) {
    return undefined;
  }

  const joinOptionsByKey = new Map(
    ctx.joinOptions.map((option) => [joinPathKey(option.path), option]),
  );
  const seenAliases = new Set<string>();

  const result = joins
    .map((join, index) => {
      const key = joinPathKey(join.path);
      const option = joinOptionsByKey.get(key);
      if (!option) {
        issues.push({
          path: `joins.${index}.path`,
          message: "Join path does not match any available relation path.",
          code: "invalid_join_path",
        });
        return null;
      }

      if (join.table !== option.table) {
        issues.push({
          path: `joins.${index}.table`,
          message: `Join table must be ${option.table} for the selected path.`,
          code: "mismatched_join_table",
        });
        return null;
      }

      const alias = join.name ?? option.name;
      if (seenAliases.has(alias)) {
        issues.push({
          path: `joins.${index}.name`,
          message: `Join alias ${alias} has already been selected.`,
          code: "duplicate_join_alias",
        });
        return null;
      }
      seenAliases.add(alias);

      return {
        table: option.table,
        name: alias,
        path: option.path,
      } satisfies NonNullable<
        FindManyQuery["operationParameters"]["joins"]
      >[number];
    })
    .filter((entry): entry is NonNullable<typeof entry> => entry !== null);

  return result.length > 0 ? result : undefined;
}

function joinPathKey(path: JoinPathHop[]) {
  return path
    .map((hop) => `${hop.source.join(",")}=>${hop.target.join(",")}`)
    .join("|");
}
