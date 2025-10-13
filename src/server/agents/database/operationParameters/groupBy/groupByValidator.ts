import { z } from "zod";
import type { GroupByQuery } from "~/server/userDatabaseConnector/types";

export interface GroupByJoinOption {
  table: string;
  joinPath: string;
}

export interface GroupByValidatorContext {
  baseTableName: string;
  joinOptions: GroupByJoinOption[]; // possible joins (1 hop)
  allPossibleColumns: string[]; // fully qualified table.column (base + all joins)
  numericalColumns: string[]; // subset of allPossibleColumns that are numeric/computed numeric
}

export interface GroupByValidResult {
  status: "valid";
  result: GroupByQuery["operationParameters"];
}

export interface GroupByInvalidResultIssue {
  path: string;
  message: string;
  code: string;
}

export interface GroupByInvalidResult {
  status: "invalid";
  issues: GroupByInvalidResultIssue[];
}

export type GroupByValidationResult = GroupByValidResult | GroupByInvalidResult;

// Build dynamic zod schema (broad superset); fine-grained rules enforced manually afterwards
export function buildGroupByZodSchema(ctx: GroupByValidatorContext) {
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
    .nullable()
    .describe("Chosen joins (subset of available); null or empty for none");

  const colEnum = z.enum([
    ctx.allPossibleColumns[0],
    ...ctx.allPossibleColumns.slice(1),
  ] as [string, ...string[]]);
  const numColEnum = ctx.numericalColumns.length
    ? z.enum([ctx.numericalColumns[0], ...ctx.numericalColumns.slice(1)] as [
        string,
        ...string[]
      ])
    : z.never();

  const aggObject = (e: typeof colEnum | typeof numColEnum) =>
    z
      .object({
        columns: z
          .array(e)
          .min(1)
          .describe("Columns for this aggregate (fully qualified)"),
      })
      .nullable();

  return z.object({
    joins: joinsSchema.optional(),
    groupBy: z
      .array(colEnum)
      .min(1)
      .describe("Columns to group by (fully qualified)"),
    count: aggObject(colEnum),
    sum: ctx.numericalColumns.length ? aggObject(numColEnum) : z.null(),
    min: ctx.numericalColumns.length ? aggObject(numColEnum) : z.null(),
    max: ctx.numericalColumns.length ? aggObject(numColEnum) : z.null(),
    avg: ctx.numericalColumns.length ? aggObject(numColEnum) : z.null(),
    orderBy: z.object({
      function: z.enum(
        ctx.numericalColumns.length
          ? (["count", "sum", "min", "max", "avg"] as const)
          : (["count"] as const)
      ),
      column: colEnum.describe("Column referenced by order by function"),
      direction: z.enum(["asc", "desc"]),
    }),
    limit: z.number().int().positive().max(1000),
  });
}

export function validateGroupByCandidate(
  candidate: unknown,
  ctx: GroupByValidatorContext
): GroupByValidationResult {
  const schema = buildGroupByZodSchema(ctx);
  const parsed = schema.safeParse(candidate);
  if (!parsed.success) {
    const issues: GroupByInvalidResultIssue[] = parsed.error.issues.map(
      (i) => ({
        path: i.path.join(".") || "<root>",
        message: i.message,
        code: i.code,
      })
    );
    return { status: "invalid", issues };
  }

  const data = parsed.data;

  // Type the joins properly to avoid unsafe access
  const joins = data.joins as Array<{
    table: string;
    joinPath: string;
    joinType: "inner" | "left" | "right";
  }> | null | undefined;

  const selectedJoinTables = new Set([
    ctx.baseTableName,
    ...(joins?.map((j) => j.table) ?? []),
  ]);

  const issues: GroupByInvalidResultIssue[] = [];

  // Helper to check table portion
  const tablePart = (fq: string) => fq.split(".")[0] ?? "";

  const ensureColumnTablesAllowed = (
    cols: string[] | undefined,
    path: string
  ) => {
    if (!cols) return;
    cols.forEach((c) => {
      if (!selectedJoinTables.has(tablePart(c))) {
        issues.push({
          path,
          message: `Column ${c} references table not selected in joins`,
          code: "unselected_table",
        });
      }
    });
  };

  ensureColumnTablesAllowed(data.groupBy, "groupBy");
  ensureColumnTablesAllowed(data.count?.columns, "count.columns");
  ensureColumnTablesAllowed(data.sum?.columns, "sum.columns");
  ensureColumnTablesAllowed(data.min?.columns, "min.columns");
  ensureColumnTablesAllowed(data.max?.columns, "max.columns");
  ensureColumnTablesAllowed(data.avg?.columns, "avg.columns");

  // orderBy validation
  if (data.orderBy.function === "count") {
    if (!data.groupBy.includes(data.orderBy.column)) {
      issues.push({
        path: "orderBy.column",
        message:
          "When ordering by count, column must be one of groupBy columns",
        code: "orderBy_invalid_column",
      });
    }
  } else {
    if (!ctx.numericalColumns.includes(data.orderBy.column)) {
      issues.push({
        path: "orderBy.column",
        message: `Column ${data.orderBy.column} is not numerical for function ${data.orderBy.function}`,
        code: "orderBy_not_numeric",
      });
    }
  }

  // Column table selection for orderBy
  if (!selectedJoinTables.has(tablePart(data.orderBy.column))) {
    issues.push({
      path: "orderBy.column",
      message: `OrderBy column table not in selected joins`,
      code: "orderBy_table_missing",
    });
  }

  if (issues.length) {
    return { status: "invalid", issues };
  }

  // Strip empty aggregate columns arrays (should not happen due to min(1), but defensively)
  const cleanAgg = (agg: { columns: string[] } | null | undefined) =>
    agg?.columns && agg.columns.length > 0 ? agg : null;

  const result: GroupByQuery["operationParameters"] = {
    joins: data.joins && data.joins.length > 0 ? data.joins : null,
    groupBy: data.groupBy,
    count: cleanAgg(data.count ?? null),
    sum: cleanAgg(data.sum ?? null),
    min: cleanAgg(data.min ?? null),
    max: cleanAgg(data.max ?? null),
    avg: cleanAgg(data.avg ?? null),
    orderBy: data.orderBy,
    limit: data.limit,
  };

  return { status: "valid", result };
}
