import { z } from "zod";
import type {
  GroupByKey,
  GroupByOrderBy,
  GroupByQuery,
} from "~/server/userDatabaseConnector/types";
import { stringArrayToZodEnum } from "~/server/agents/utils/zodHelpers";

export interface GroupByJoinOption {
  table: string;
  joinPath: string;
}

export interface GroupByValidatorContext {
  baseTableName: string;
  joinOptions: GroupByJoinOption[]; // possible joins (1 hop)
  allColumns: string[]; // fully qualified table.column (base + all joins)
  groupableColumns: string[]; // subset eligible for plain column grouping (non-temporal)
  intervalColumns: string[]; // subset eligible for date interval grouping
  numericalColumns: string[]; // subset of allColumns that are numeric/computed numeric
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

  const buildEnum = (values: string[]) =>
    values.length ? stringArrayToZodEnum(values) : null;

  const allColumnsEnum = buildEnum(ctx.allColumns);
  const numericalColumnsEnum = buildEnum(ctx.numericalColumns);
  const groupableColumnsEnum = buildEnum(ctx.groupableColumns);
  const intervalColumnsEnum = buildEnum(ctx.intervalColumns);

  const allColumnsEnumOrNever = allColumnsEnum ?? z.never();

  const buildAggregateArraySchema = (
    enumSchema: z.ZodEnum<[string, ...string[]]> | null
  ) =>
    enumSchema
      ? z
          .array(enumSchema)
          .min(1)
          .describe("Columns for this aggregate (fully qualified)")
          .nullable()
      : z.null();

  type ColumnGroupKey = Extract<GroupByKey, { type: "column" }>;
  type IntervalGroupKey = Extract<GroupByKey, { type: "dateInterval" }>;

  const buildColumnGroupKeySchema = (
    columnEnum: z.ZodEnum<[string, ...string[]]>
  ): z.ZodType<ColumnGroupKey> =>
    z
      .object({
        type: z.literal("column"),
        column: columnEnum,
        alias: z.string().min(1).optional(),
      })
      .strict();

  const buildIntervalGroupKeySchema = (
    columnEnum: z.ZodEnum<[string, ...string[]]>
  ): z.ZodType<IntervalGroupKey> =>
    z
      .object({
        type: z.literal("dateInterval"),
        column: columnEnum,
        interval: z.enum(["day", "week", "month", "quarter", "year", "hour"]),
        alias: z.string().min(1).optional(),
      })
      .strict();

  const columnKeySchema = groupableColumnsEnum
    ? buildColumnGroupKeySchema(groupableColumnsEnum)
    : null;

  const intervalKeySchema = intervalColumnsEnum
    ? buildIntervalGroupKeySchema(intervalColumnsEnum)
    : null;

  let groupByKeySchema: z.ZodType<GroupByKey>;
  if (columnKeySchema && intervalKeySchema) {
    groupByKeySchema = z.union([columnKeySchema, intervalKeySchema]);
  } else if (columnKeySchema) {
    groupByKeySchema = columnKeySchema;
  } else if (intervalKeySchema) {
    groupByKeySchema = intervalKeySchema;
  } else {
    groupByKeySchema = z.never();
  }

  const groupKeyOrderSchema = z
    .object({
      type: z.literal("groupKey"),
      key: z.string(),
      direction: z.enum(["asc", "desc"]),
    })
    .strict();

  const aggregateOrderSchema = z
    .object({
      type: z.literal("aggregate"),
      function: z.enum(
        ctx.numericalColumns.length
          ? (["count", "sum", "min", "max", "avg"] as const)
          : (["count"] as const)
      ),
      column: allColumnsEnumOrNever.describe(
        "Column referenced by order by function"
      ),
      direction: z.enum(["asc", "desc"]),
    })
    .strict();

  return z.object({
    joins: joinsSchema.optional(),
    groupBy: z
      .array(groupByKeySchema)
      .min(1)
      .describe("Columns or interval keys to group by"),
    count: buildAggregateArraySchema(allColumnsEnum),
    sum: buildAggregateArraySchema(numericalColumnsEnum),
    min: buildAggregateArraySchema(numericalColumnsEnum),
    max: buildAggregateArraySchema(numericalColumnsEnum),
    avg: buildAggregateArraySchema(numericalColumnsEnum),
    orderBy: z.union([groupKeyOrderSchema, aggregateOrderSchema]),
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

  const data = parsed.data as {
    joins?: GroupByQuery["operationParameters"]["joins"];
    groupBy: GroupByKey[];
    count: string[] | null;
    sum: string[] | null;
    min: string[] | null;
    max: string[] | null;
    avg: string[] | null;
    orderBy: GroupByOrderBy;
    limit: number;
  };

  // Type the joins properly to avoid unsafe access
  const joins = data.joins as
    | Array<{
        table: string;
        joinPath: string;
        joinType: "inner" | "left" | "right";
      }>
    | null
    | undefined;

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

  const resolvedGroupBy: GroupByKey[] = [];
  const aliasSet = new Set<string>();
  data.groupBy.forEach((key, index) => {
    const aliasCandidate =
      key.alias && key.alias.trim().length > 0
        ? key.alias.trim()
        : key.type === "column"
        ? key.column
        : `${key.column}|${key.interval}`;

    if (aliasSet.has(aliasCandidate)) {
      issues.push({
        path: `groupBy[${index}].alias`,
        message: `Alias ${aliasCandidate} is used more than once`,
        code: "duplicate_alias",
      });
    } else {
      aliasSet.add(aliasCandidate);
    }

    const groupByTable = tablePart(key.column);
    if (!selectedJoinTables.has(groupByTable)) {
      issues.push({
        path: `groupBy[${index}].column`,
        message: `Column ${key.column} references table not selected in joins`,
        code: "unselected_table",
      });
    }

    if (key.type === "column" && ctx.intervalColumns.includes(key.column)) {
      issues.push({
        path: `groupBy[${index}].column`,
        message: `Column ${key.column} is temporal and must use dateInterval grouping`,
        code: "invalid_column_grouping",
      });
    }

    if (
      key.type === "dateInterval" &&
      !ctx.intervalColumns.includes(key.column)
    ) {
      issues.push({
        path: `groupBy[${index}].column`,
        message: `Column ${key.column} must be a date/time column for interval grouping`,
        code: "invalid_interval_column",
      });
    }

    resolvedGroupBy.push({
      ...key,
      alias: aliasCandidate,
    });
  });

  ensureColumnTablesAllowed(data.count ?? undefined, "count");
  ensureColumnTablesAllowed(data.sum ?? undefined, "sum");
  ensureColumnTablesAllowed(data.min ?? undefined, "min");
  ensureColumnTablesAllowed(data.max ?? undefined, "max");
  ensureColumnTablesAllowed(data.avg ?? undefined, "avg");

  // orderBy validation
  if (data.orderBy.type === "groupKey") {
    if (!aliasSet.has(data.orderBy.key)) {
      issues.push({
        path: "orderBy.key",
        message: `OrderBy key ${data.orderBy.key} must match a groupBy alias`,
        code: "orderBy_key_missing",
      });
    }
  } else {
    if (
      data.orderBy.function !== "count" &&
      !ctx.numericalColumns.includes(data.orderBy.column)
    ) {
      issues.push({
        path: "orderBy.column",
        message: `Column ${data.orderBy.column} is not numerical for function ${data.orderBy.function}`,
        code: "orderBy_not_numeric",
      });
    }

    const allowedTables = selectedJoinTables;
    if (!allowedTables.has(tablePart(data.orderBy.column))) {
      issues.push({
        path: "orderBy.column",
        message: `OrderBy column table not in selected joins`,
        code: "orderBy_table_missing",
      });
    }

    const aggregateColumnsByFunction: Record<
      "count" | "sum" | "min" | "max" | "avg",
      string[] | null | undefined
    > = {
      count: data.count,
      sum: data.sum,
      min: data.min,
      max: data.max,
      avg: data.avg,
    };

    const targetColumns = aggregateColumnsByFunction[data.orderBy.function];

    if (!targetColumns?.includes(data.orderBy.column)) {
      issues.push({
        path: "orderBy.column",
        message: `Column ${data.orderBy.column} must be included in the ${data.orderBy.function} aggregate columns`,
        code: "orderBy_column_not_aggregated",
      });
    }
  }

  if (issues.length) {
    return { status: "invalid", issues };
  }

  // Strip empty aggregate columns arrays (should not happen due to min(1), but defensively)
  const cleanAgg = (agg: string[] | null | undefined) =>
    agg && agg.length > 0 ? agg : null;

  const result: GroupByQuery["operationParameters"] = {
    joins: data.joins && data.joins.length > 0 ? data.joins : null,
    groupBy: resolvedGroupBy,
    count: cleanAgg(data.count),
    sum: cleanAgg(data.sum),
    min: cleanAgg(data.min),
    max: cleanAgg(data.max),
    avg: cleanAgg(data.avg),
    orderBy:
      data.orderBy.type === "groupKey"
        ? {
            type: "groupKey",
            key: data.orderBy.key,
            direction: data.orderBy.direction,
          }
        : {
            type: "aggregate",
            function: data.orderBy.function,
            column: data.orderBy.column,
            direction: data.orderBy.direction,
          },
    limit: data.limit,
  };

  return { status: "valid", result };
}
