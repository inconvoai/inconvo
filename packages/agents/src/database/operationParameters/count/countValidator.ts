import { z } from "zod";
import {
  joinDescriptorSchema,
  type CountQuery,
  type JoinPathHop,
} from "@repo/types";
import { stringArrayToZodEnum } from "../../../utils/zodHelpers";

const countJoinSchema = joinDescriptorSchema.strip();

type CountJoinInput = z.infer<typeof countJoinSchema>;

export interface CountJoinOption {
  name: string;
  table: string;
  path: JoinPathHop[];
  selectableColumns: string[];
}

export interface CountValidatorContext {
  baseTable: string;
  baseColumns: string[];
  computedColumns: string[];
  joinOptions: CountJoinOption[];
}

export interface CountValidResult {
  status: "valid";
  result: CountQuery["operationParameters"];
}

export interface CountInvalidIssue {
  path: string;
  message: string;
  code: string;
}

export interface CountInvalidResult {
  status: "invalid";
  issues: CountInvalidIssue[];
}

export type CountValidationResult = CountValidResult | CountInvalidResult;

export function buildCountToolZodSchema(ctx: CountValidatorContext) {
  const joinDescription =
    ctx.joinOptions.length > 0
      ? [
          "Provide joins when counting columns from related tables.",
          "Available joins:",
          ...ctx.joinOptions.map(
            (option) =>
              `${option.name} (${option.table}) path=${formatJoinPath(
                option.path,
              )}`,
          ),
        ].join("\n")
      : "Provide joins when counting columns from related tables.";

  const baseColumnExamples = [
    ...ctx.baseColumns.map((column) => `${ctx.baseTable}.${column}`),
    ...ctx.computedColumns.map((column) => `${ctx.baseTable}.${column}`),
  ];

  const aliasExamples = ctx.joinOptions.map((option) => option.name);
  const joinedColumnGuidance =
    aliasExamples.length > 0
      ? `Joined columns must reference a join alias (alias.column). Available aliases after joining: ${aliasExamples.join(
          ", ",
        )}.`
      : "Joined columns must reference a join alias (alias.column).";

  const commonDescriptionSegments = (scope: "count" | "countDistinct") =>
    [
      scope === "count"
        ? "Columns to count. Use `_all` for COUNT(*). Counting a column sums every row produced by the joins; use `countDistinct` when you need unique values. Base columns must be table.column."
        : "Columns to count distinct values for. Prefer this when you only want unique values from the joined rows. Base columns must be table.column.",
      baseColumnExamples.length > 0
        ? `Examples of base columns: ${baseColumnExamples.join(", ")}.`
        : undefined,
      joinedColumnGuidance,
    ]
      .filter(Boolean)
      .join(" ");

  const countArraySchema = z
    .array(z.string().min(1))
    .min(1, "Select at least one column to count");

  const countDistinctArraySchema = z
    .array(z.string().min(1))
    .min(1, "Select at least one column to count distinct values for");

  return z
    .object({
      joins: z.array(countJoinSchema).nullable().describe(joinDescription),
      count: countArraySchema
        .nullable()
        .optional()
        .default(null)
        .describe(commonDescriptionSegments("count")),
      countDistinct: countDistinctArraySchema
        .nullable()
        .optional()
        .default(null)
        .describe(commonDescriptionSegments("countDistinct")),
    })
    .superRefine((data, issueCtx) => {
      const hasCount = Array.isArray(data.count) && data.count.length > 0;
      const hasDistinct =
        Array.isArray(data.countDistinct) && data.countDistinct.length > 0;

      if (!hasCount && !hasDistinct) {
        issueCtx.addIssue({
          code: z.ZodIssueCode.custom,
          message:
            "Provide at least one metric via `count` or `countDistinct`.",
          path: ["count"],
        });
        issueCtx.addIssue({
          code: z.ZodIssueCode.custom,
          message:
            "Provide at least one metric via `count` or `countDistinct`.",
          path: ["countDistinct"],
        });
      }
    });
}

export function buildCountValidatorZodSchema(ctx: CountValidatorContext) {
  const baseColumns = new Set<string>();
  ctx.baseColumns.forEach((column) => {
    baseColumns.add(`${ctx.baseTable}.${column}`);
  });
  ctx.computedColumns.forEach((column) => {
    baseColumns.add(`${ctx.baseTable}.${column}`);
  });

  const joinColumnTokens = new Set<string>();
  ctx.joinOptions.forEach((option) => {
    option.selectableColumns.forEach((column) => {
      joinColumnTokens.add(`${option.name}.${column}`);
    });
  });

  const countColumnEnum = stringArrayToZodEnum([
    "_all",
    ...Array.from(baseColumns),
    ...Array.from(joinColumnTokens),
  ]);

  const countDistinctEnum = stringArrayToZodEnum([
    ...Array.from(baseColumns),
    ...Array.from(joinColumnTokens),
  ]);

  const joinDescription =
    ctx.joinOptions.length > 0
      ? [
          "Provide joins when counting columns from related tables.",
          "Available joins:",
          ...ctx.joinOptions.map(
            (option) =>
              `${option.name} (${option.table}) path=${formatJoinPath(
                option.path,
              )}`,
          ),
        ].join("\n")
      : "Provide joins when counting columns from related tables.";

  const countEnumArraySchema = z
    .array(countColumnEnum)
    .min(1, "Select at least one column to count");

  const countDistinctEnumArraySchema = z
    .array(countDistinctEnum)
    .min(1, "Select at least one column to count distinct values for");

  return z
    .object({
      joins: z
        .array(countJoinSchema)
        .nullable()
        .optional()
        .describe(joinDescription),
      count: countEnumArraySchema
        .nullable()
        .optional()
        .default(null)
        .describe(
          "Columns to count. Use `_all` for COUNT(*). Counting a column sums every row produced by the joins; use `countDistinct` for unique values. Base columns must be table.column; joined columns must be alias.column.",
        ),
      countDistinct: countDistinctEnumArraySchema
        .nullable()
        .optional()
        .default(null)
        .describe(
          "Columns to count distinct values for. Prefer this when only unique values are needed. Base columns must be table.column; joined columns must be alias.column.",
        ),
    })
    .superRefine((data, issueCtx) => {
      const hasCount = Array.isArray(data.count) && data.count.length > 0;
      const hasDistinct =
        Array.isArray(data.countDistinct) && data.countDistinct.length > 0;

      if (!hasCount && !hasDistinct) {
        issueCtx.addIssue({
          code: z.ZodIssueCode.custom,
          message:
            "Provide at least one metric via `count` or `countDistinct`.",
          path: ["count"],
        });
        issueCtx.addIssue({
          code: z.ZodIssueCode.custom,
          message:
            "Provide at least one metric via `count` or `countDistinct`.",
          path: ["countDistinct"],
        });
      }
    });
}

export function validateCountCandidate(
  candidate: unknown,
  ctx: CountValidatorContext,
): CountValidationResult {
  const schema = buildCountValidatorZodSchema(ctx);
  const parsed = schema.safeParse(candidate);
  if (!parsed.success) {
    return {
      status: "invalid",
      issues: parsed.error.issues.map((issue) => ({
        path: issue.path.join(".") || "<root>",
        message: issue.message,
        code: issue.code,
      })),
    };
  }

  const data = parsed.data;
  const issues: CountInvalidIssue[] = [];
  const baseColumnSet = new Set([...ctx.baseColumns, ...ctx.computedColumns]);

  const joinsInput =
    "joins" in data
      ? ((data as { joins?: CountJoinInput[] | null }).joins ?? null)
      : null;

  const validatedJoins = validateJoins(joinsInput, ctx, issues);

  const joinAliasSet = new Set<string>();
  (validatedJoins ?? []).forEach((join) =>
    joinAliasSet.add(join.name ?? join.table),
  );

  const checkColumnReference = (column: string, pathPrefix: string) => {
    if (column === "_all") {
      return;
    }

    const resolution = resolveColumnReference(column, ctx, baseColumnSet);

    if (!resolution.success) {
      issues.push({
        path: pathPrefix,
        message: resolution.error.message,
        code: resolution.error.code,
      });
      return;
    }

    const reference = resolution.reference;
    if (reference.scope === "join" && !joinAliasSet.has(reference.alias)) {
      issues.push({
        path: pathPrefix,
        message: `Join alias ${reference.alias} must be included in the joins array.`,
        code: "missing_join",
      });
    }
  };

  data.count?.forEach((column, index) => {
    checkColumnReference(column, `count.${index}`);
  });

  data.countDistinct?.forEach((column, index) => {
    checkColumnReference(column, `countDistinct.${index}`);
  });

  if (issues.length > 0) {
    return { status: "invalid", issues };
  }

  return {
    status: "valid",
    result: {
      joins: validatedJoins ?? null,
      count: data.count ?? null,
      countDistinct: data.countDistinct ?? null,
    },
  };
}

interface ResolvedColumnReference {
  scope: "base" | "join";
  alias: string;
  column: string;
}

interface ColumnReferenceError {
  code: string;
  message: string;
}

type ColumnReferenceResolution =
  | { success: true; reference: ResolvedColumnReference }
  | { success: false; error: ColumnReferenceError };

function resolveColumnReference(
  rawColumn: string,
  ctx: CountValidatorContext,
  baseColumnSet: Set<string>,
): ColumnReferenceResolution {
  if (rawColumn === "_all") {
    return {
      success: true,
      reference: {
        scope: "base",
        alias: ctx.baseTable,
        column: "_all",
      },
    };
  }

  const [aliasPart, columnPart] = splitAliasAndColumn(rawColumn);
  if (!aliasPart || !columnPart) {
    return {
      success: false,
      error: {
        code: "invalid_format",
        message: `Column ${rawColumn} must be qualified as alias.column.`,
      },
    };
  }

  if (aliasPart === ctx.baseTable) {
    if (baseColumnSet.has(columnPart)) {
      return {
        success: true,
        reference: {
          scope: "base",
          alias: ctx.baseTable,
          column: columnPart,
        },
      };
    }
    return {
      success: false,
      error: {
        code: "invalid_column",
        message: `Column ${rawColumn} is not valid for counting.`,
      },
    };
  }

  const matchingOption = ctx.joinOptions.find(
    (option) => option.name === aliasPart,
  );

  if (!matchingOption) {
    return {
      success: false,
      error: {
        code: aliasPart.includes(".") ? "invalid_alias_chain" : "invalid_alias",
        message: aliasPart.includes(".")
          ? `Alias ${aliasPart} refers to a nested path. Use the join alias provided in the joins array instead of chaining segments.`
          : `Join alias ${aliasPart} is not valid for counting.`,
      },
    };
  }

  if (!matchingOption.selectableColumns.includes(columnPart)) {
    return {
      success: false,
      error: {
        code: "invalid_column",
        message: `Column ${rawColumn} is not valid for counting.`,
      },
    };
  }

  return {
    success: true,
    reference: {
      scope: "join",
      alias: matchingOption.name,
      column: columnPart,
    },
  };
}

function splitAliasAndColumn(column: string): [string, string | null] {
  const lastDot = column.lastIndexOf(".");
  if (lastDot === -1) {
    return [column, null];
  }
  const alias = column.slice(0, lastDot);
  const col = column.slice(lastDot + 1);
  return [alias, col || null];
}

function validateJoins(
  joins: CountJoinInput[] | null,
  ctx: CountValidatorContext,
  issues: CountInvalidIssue[],
): CountQuery["operationParameters"]["joins"] {
  if (!joins || joins.length === 0) {
    return undefined;
  }

  const optionsByKey = new Map(
    ctx.joinOptions.map((option) => [joinPathKey(option.path), option]),
  );
  const seenAliases = new Set<string>([ctx.baseTable]);

  const validated = joins
    .map((join, index) => {
      const key = joinPathKey(join.path);
      const option = optionsByKey.get(key);
      if (!option) {
        issues.push({
          path: `joins.${index}.path`,
          message: "Join path does not match any available relation path.",
          code: "invalid_join_path",
        });
        return null;
      }

      const alias = join.name ?? option.name;
      if (seenAliases.has(alias)) {
        issues.push({
          path: `joins.${index}.name`,
          message:
            alias === ctx.baseTable
              ? `Join alias ${alias} conflicts with the base table alias.`
              : `Join alias ${alias} already provided.`,
          code: "duplicate_join_alias",
        });
        return null;
      }

      if (join.table !== option.table) {
        issues.push({
          path: `joins.${index}.table`,
          message: `Join table must be ${option.table} for this path.`,
          code: "invalid_join_table",
        });
        return null;
      }

      seenAliases.add(alias);

      return {
        table: option.table,
        name: alias,
        path: option.path,
        joinType: join.joinType,
      } satisfies NonNullable<
        CountQuery["operationParameters"]["joins"]
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
    .map((hop) => `[${hop.source.join(", ")}] -> [${hop.target.join(", ")}]`)
    .join(" | ");
}
