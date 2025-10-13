import { z } from "zod";
import { stringArrayToZodEnum } from "../../../utils/zodHelpers";
import type { FindManyQuery } from "~/server/userDatabaseConnector/types";

export interface FindManyValidatorContext {
  selectableTableColumns: Record<string, string[]>; // key = join path, value = columns (including computed)
  baseColumns: string[];
  baseComputedColumns: string[];
}

export interface FindManyValidResult {
  status: "valid";
  result: {
    columns: Record<string, string[]>; // filtered (no empty arrays)
    orderBy: { direction: "asc" | "desc"; column: string } | null;
    limit: number;
  };
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
  | FindManyValidResult
  | FindManyInvalidResult;

/**
 * Build the dynamic Zod schema for findMany operation parameters based on the selectable tables & base table columns.
 * (Matches the inline logic previously in findManySingleCall.draft.ts)
 */
export function buildFindManyZodSchema(ctx: FindManyValidatorContext) {
  const columnsValidator = z.object(
    Object.keys(ctx.selectableTableColumns).reduce(
      (
        acc: Record<
          string,
          z.ZodOptional<
            z.ZodNullable<z.ZodArray<z.ZodEnum<[string, ...string[]]>>>
          >
        >,
        key
      ) => {
        const cols = ctx.selectableTableColumns[key];
        if (!cols?.length) return acc;
        acc[key] = z.array(stringArrayToZodEnum(cols)).nullable().optional();
        return acc;
      },
      {}
    )
  );

  return z.object({
    columns: columnsValidator.describe(
      "Keys are join path identifiers. Only include paths you need. Omit or null unused ones."
    ),
    orderBy: z
      .object({
        direction: z.enum(["asc", "desc"]),
        column: stringArrayToZodEnum([
          ...ctx.baseColumns,
          ...ctx.baseComputedColumns,
        ]).describe("Ordering column (must come from base table)"),
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

/**
 * Pure validation + cleaning (strips empty arrays) for a candidate findMany params object.
 * Returns structured success / failure without any tool wrapper – easy to unit test.
 */
export function validateFindManyCandidate(
  candidate: unknown,
  ctx: FindManyValidatorContext
): FindManyValidationResult {
  const schema = buildFindManyZodSchema(ctx);
  const parsed = schema.safeParse(candidate);
  if (!parsed.success) {
    const issues: FindManyInvalidResultIssue[] = parsed.error.issues.map(
      (i) => ({
        path: i.path.join(".") || "<root>",
        message: i.message,
        code: i.code,
      })
    );
    return { status: "invalid", issues };
  }
  const filteredColumns = Object.entries(parsed.data.columns).reduce(
    (acc: Record<string, string[]>, [k, v]) => {
      if (Array.isArray(v) && v.length > 0) acc[k] = v;
      return acc;
    },
    {}
  );
  return {
    status: "valid",
    result: {
      ...parsed.data,
      columns: filteredColumns,
    } satisfies FindManyQuery["operationParameters"],
  };
}
