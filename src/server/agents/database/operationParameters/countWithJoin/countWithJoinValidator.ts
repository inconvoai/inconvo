import { z } from "zod";
import type { CountWithJoinQuery } from "~/server/userDatabaseConnector/types";

export interface CountWithJoinValidatorContext {
  baseTable: string;
  joinOptions: { table: string; joinPath: string }[]; // possible joins (1 hop)
  allPossibleColumns: string[]; // fully qualified columns (table.column)
}

export interface CountWithJoinValidResult {
  status: "valid";
  result: CountWithJoinQuery["operationParameters"]; // shape { joins: [...], count: string[], countDistinct: string[] | null }
}

export interface CountWithJoinInvalidResultIssue {
  path: string;
  message: string;
  code: string;
}

export interface CountWithJoinInvalidResult {
  status: "invalid";
  issues: CountWithJoinInvalidResultIssue[];
}

export type CountWithJoinValidationResult =
  | CountWithJoinValidResult
  | CountWithJoinInvalidResult;

export function buildCountWithJoinZodSchema(
  ctx: CountWithJoinValidatorContext
) {
  const joinLiteralSchemas = ctx.joinOptions.map((opt) =>
    z.object({
      table: z.literal(opt.table),
      joinPath: z.literal(opt.joinPath),
      joinType: z.enum(["inner", "left", "right"]),
    })
  );

  const joinsSchema = z
    .array(
      z.union(
        joinLiteralSchemas as unknown as [
          z.ZodTypeAny,
          z.ZodTypeAny,
          ...z.ZodTypeAny[]
        ]
      )
    )
    .min(1, "At least one join required")
    .describe("Selected joins to traverse from base table");

  const colEnum = z.enum([
    ctx.allPossibleColumns[0],
    ...ctx.allPossibleColumns.slice(1),
  ] as [string, ...string[]]);

  return z.object({
    joins: joinsSchema,
    count: z.array(colEnum).min(1),
    countDistinct: z.array(colEnum).nullable(),
  });
}

export function validateCountWithJoinCandidate(
  candidateOperationParameters: unknown,
  ctx: CountWithJoinValidatorContext
): CountWithJoinValidationResult {
  const schema = buildCountWithJoinZodSchema(ctx);
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

  // Ensure columns reference only selected tables
  // Type the joins properly to avoid unsafe access
  const joins = parsed.data.joins as Array<{
    table: string;
    joinPath: string;
    joinType: "inner" | "left" | "right";
  }>;

  const selectedTables = new Set([
    ctx.baseTable,
    ...joins.map((j) => j.table),
  ]);

  const issues: CountWithJoinInvalidResultIssue[] = [];
  const tableOf = (fq: string) => fq.split(".")[0] ?? "";

  [...parsed.data.count, ...(parsed.data.countDistinct ?? [])].forEach(
    (col) => {
      if (!selectedTables.has(tableOf(col))) {
        issues.push({
          path: "count",
          message: `Column ${col} references unjoined table`,
          code: "unjoined_table",
        });
      }
    }
  );

  if (issues.length) return { status: "invalid", issues };

  const result: CountWithJoinQuery["operationParameters"] = {
    joins: joins,
    count: parsed.data.count,
    countDistinct: parsed.data.countDistinct ?? null,
  };
  return { status: "valid", result };
}
