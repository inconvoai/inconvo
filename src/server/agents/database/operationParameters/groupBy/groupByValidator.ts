import { z } from "zod";
import {
  joinDescriptorSchema,
  joinPathHopSchema,
  joinTypeSchema,
  type GroupByKey,
  type GroupByQuery,
  type JoinPathHop,
} from "~/server/userDatabaseConnector/types";
import { stringArrayToZodEnum } from "~/server/agents/utils/zodHelpers";

export interface GroupByJoinOption {
  name: string;
  table: string;
  path: JoinPathHop[];
}

export interface GroupByValidatorContext {
  baseTableName: string;
  joinOptions: GroupByJoinOption[];
  allColumns: string[];
  groupableColumns: string[];
  intervalColumns: string[];
  numericalColumns: string[];
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

export type GroupByValidationResult =
  | { status: "valid"; result: GroupByQuery["operationParameters"] }
  | GroupByInvalidResult;

const joinSchema = joinDescriptorSchema
  .extend({
    path: z.array(joinPathHopSchema).min(1),
    joinType: joinTypeSchema.optional(),
  })
  .strip();

export function buildGroupByZodSchema(ctx: GroupByValidatorContext) {
  const buildEnum = (values: string[]) =>
    values.length ? stringArrayToZodEnum(values) : null;

  const allColumnsEnum = buildEnum(ctx.allColumns);
  const numericalColumnsEnum = buildEnum(ctx.numericalColumns);
  const groupableColumnsEnum = buildEnum(ctx.groupableColumns);
  const intervalColumnsEnum = buildEnum(ctx.intervalColumns);

  const buildAggregateArraySchema = (
    enumSchema: z.ZodEnum<[string, ...string[]]> | null
  ) => (enumSchema ? z.array(enumSchema).min(1).nullable() : z.null());

  type ColumnGroupKey = Extract<GroupByKey, { type: "column" }>;
  type IntervalGroupKey = Extract<GroupByKey, { type: "dateInterval" }>;
  type ComponentGroupKey = Extract<GroupByKey, { type: "dateComponent" }>;

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

  const buildComponentGroupKeySchema = (
    columnEnum: z.ZodEnum<[string, ...string[]]>
  ): z.ZodType<ComponentGroupKey> =>
    z
      .object({
        type: z.literal("dateComponent"),
        column: columnEnum,
        component: z.enum(["dayOfWeek", "monthOfYear", "quarterOfYear"]),
        alias: z.string().min(1).optional(),
      })
      .strict();

  const keySchemas: z.ZodTypeAny[] = [];
  if (groupableColumnsEnum)
    keySchemas.push(buildColumnGroupKeySchema(groupableColumnsEnum));
  if (intervalColumnsEnum) {
    keySchemas.push(buildIntervalGroupKeySchema(intervalColumnsEnum));
    keySchemas.push(buildComponentGroupKeySchema(intervalColumnsEnum));
  }

  let groupByKeySchema: z.ZodType<GroupByKey>;
  if (keySchemas.length === 0) {
    groupByKeySchema = z.never();
  } else if (keySchemas.length === 1) {
    groupByKeySchema = keySchemas[0] as z.ZodType<GroupByKey>;
  } else {
    groupByKeySchema = z.union(
      keySchemas as [z.ZodTypeAny, z.ZodTypeAny, ...z.ZodTypeAny[]]
    ) as z.ZodType<GroupByKey>;
  }

  const allColumnsEnumOrNever = allColumnsEnum ?? z.never();

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
      function: z.enum(["count", "sum", "min", "max", "avg"]),
      column: allColumnsEnumOrNever,
      direction: z.enum(["asc", "desc"]),
    })
    .strict();

  return z.object({
    joins: z.array(joinSchema).nullable().optional(),
    groupBy: z.array(groupByKeySchema).min(1),
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
      (issue) => ({
        path: issue.path.join(".") || "<root>",
        message: issue.message,
        code: issue.code,
      })
    );
    return { status: "invalid", issues };
  }

  const data = parsed.data;

  const issues: GroupByInvalidResultIssue[] = [];
  const validatedJoins = validateJoins(data.joins ?? null, ctx, issues);

  if (issues.length) {
    return { status: "invalid", issues };
  }

  const aliasToTable = new Map<string, string>();
  aliasToTable.set(ctx.baseTableName, ctx.baseTableName);
  validatedJoins?.forEach((join) => {
    const alias = join.name ?? join.table;
    aliasToTable.set(alias, join.table);
    aliasToTable.set(join.table, join.table);
  });

  const selectedJoinTables = new Set(aliasToTable.keys());

  const tablePart = (fq: string) => fq.split(".")[0] ?? "";

  const ensureColumnTablesAllowed = (
    cols: string[] | undefined,
    path: string
  ) => {
    if (!cols) return;
    cols.forEach((c) => {
      const tableAlias = tablePart(c);
      if (!selectedJoinTables.has(tableAlias)) {
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
    const defaultAlias =
      key.type === "column"
        ? key.column
        : key.type === "dateInterval"
          ? `${key.column}|${key.interval}`
          : `${key.column}|${key.component}`;
    const aliasCandidate = (key.alias ?? defaultAlias).trim() || defaultAlias;

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
        message: `Column ${key.column} is temporal and must use dateInterval or dateComponent grouping`,
        code: "invalid_column_grouping",
      });
    }

    if (
      (key.type === "dateInterval" || key.type === "dateComponent") &&
      !ctx.intervalColumns.includes(key.column)
    ) {
      issues.push({
        path: `groupBy[${index}].column`,
        message: `Column ${key.column} must be a date/time column for temporal grouping`,
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

    if (!selectedJoinTables.has(tablePart(data.orderBy.column))) {
      issues.push({
        path: "orderBy.column",
        message: `OrderBy column ${data.orderBy.column} references table not selected in joins`,
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

  const cleanAgg = (agg: string[] | null | undefined) =>
    agg && agg.length > 0 ? agg : null;

  const result: GroupByQuery["operationParameters"] = {
    joins: validatedJoins ?? null,
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

function validateJoins(
  joins: z.infer<typeof joinSchema>[] | null,
  ctx: GroupByValidatorContext,
  issues: GroupByInvalidResultIssue[]
): GroupByQuery["operationParameters"]["joins"] {
  if (!joins || joins.length === 0) {
    return undefined;
  }

  const joinOptionsByKey = new Map(
    ctx.joinOptions.map((option) => [joinPathKey(option.path), option])
  );
  const seenAliases = new Set<string>();

  const validated = joins
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
        joinType: join.joinType,
      } satisfies NonNullable<
        GroupByQuery["operationParameters"]["joins"]
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
