import { z } from "zod";
import {
  joinDescriptorSchema,
  joinPathHopSchema,
  joinTypeSchema,
  type AggregateGroupsQuery,
  type GroupByKey,
  type JoinPathHop,
} from "~/server/userDatabaseConnector/types";
import { stringArrayToZodEnum } from "~/server/agents/utils/zodHelpers";

export interface AggregateGroupsJoinOption {
  name: string;
  table: string;
  path: JoinPathHop[];
}

export interface AggregateGroupsValidatorContext {
  baseTableName: string;
  joinOptions: AggregateGroupsJoinOption[];
  allColumns: string[];
  groupableColumns: string[];
  intervalColumns: string[];
  numericalColumns: string[];
}

export interface AggregateGroupsInvalidResultIssue {
  path: string;
  message: string;
  code: string;
}

export interface AggregateGroupsInvalidResult {
  status: "invalid";
  issues: AggregateGroupsInvalidResultIssue[];
}

export type AggregateGroupsValidationResult =
  | { status: "valid"; result: AggregateGroupsQuery["operationParameters"] }
  | AggregateGroupsInvalidResult;

const reducerEnum = z.enum(["sum", "min", "max", "avg"]);

const joinSchema = joinDescriptorSchema
  .extend({
    path: z.array(joinPathHopSchema).min(1),
    joinType: joinTypeSchema.optional(),
  })
  .strip();

export function buildAggregateGroupsZodSchema(
  ctx: AggregateGroupsValidatorContext
) {
  const buildEnum = (values: string[]) =>
    values.length ? stringArrayToZodEnum(values) : null;

  const allColumnsEnum = buildEnum(ctx.allColumns);
  const numericalColumnsEnum = buildEnum(ctx.numericalColumns);
  const groupableColumnsEnum = buildEnum(ctx.groupableColumns);
  const intervalColumnsEnum = buildEnum(ctx.intervalColumns);

  const buildAggregateArraySchema = (
    enumSchema: z.ZodEnum<[string, ...string[]]> | null
  ) => (enumSchema ? z.array(enumSchema).nullable() : z.null());

  const havingOperatorSchema = z.enum([
    "equals",
    "not",
    "in",
    "notIn",
    "lt",
    "lte",
    "gt",
    "gte",
  ]);

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

  const havingAggregateSchema = z
    .object({
      type: z.literal("aggregate"),
      function: z.enum(["count", "countDistinct", "sum", "min", "max", "avg"]),
      column: allColumnsEnumOrNever,
      operator: havingOperatorSchema,
      value: z.any(),
    })
    .strict();

  const havingGroupKeySchema = z
    .object({
      type: z.literal("groupKey"),
      key: z.string(),
      operator: havingOperatorSchema,
      value: z.any(),
    })
    .strict();

  const havingSchema = z
    .array(z.union([havingAggregateSchema, havingGroupKeySchema]))
    .nullable()
    .optional();

  const reducersSchema = z
    .object({
      count: z.array(reducerEnum).min(1).nullable().optional(),
      countDistinct: z.array(reducerEnum).min(1).nullable().optional(),
      sum: z.array(reducerEnum).min(1).nullable().optional(),
      min: z.array(reducerEnum).min(1).nullable().optional(),
      max: z.array(reducerEnum).min(1).nullable().optional(),
      avg: z.array(reducerEnum).min(1).nullable().optional(),
    })
    .strict()
    .optional();

  return z.object({
    joins: z.array(joinSchema).nullable().optional(),
    groupBy: z.array(groupByKeySchema).min(1),
    having: havingSchema,
    aggregates: z
      .object({
        groupCount: z.boolean().optional(),
        count: buildAggregateArraySchema(allColumnsEnum).optional(),
        countDistinct: buildAggregateArraySchema(allColumnsEnum).optional(),
        sum: buildAggregateArraySchema(numericalColumnsEnum).optional(),
        min: buildAggregateArraySchema(numericalColumnsEnum).optional(),
        max: buildAggregateArraySchema(numericalColumnsEnum).optional(),
        avg: buildAggregateArraySchema(numericalColumnsEnum).optional(),
      })
      .strict(),
    reducers: reducersSchema,
  });
}

export function validateAggregateGroupsCandidate(
  candidate: unknown,
  ctx: AggregateGroupsValidatorContext
): AggregateGroupsValidationResult {
  const schema = buildAggregateGroupsZodSchema(ctx);
  const parsed = schema.safeParse(candidate);
  if (!parsed.success) {
    const issues: AggregateGroupsInvalidResultIssue[] = parsed.error.issues.map(
      (issue) => ({
        path: issue.path.join(".") || "<root>",
        message: issue.message,
        code: issue.code,
      })
    );
    return { status: "invalid", issues };
  }

  const data = parsed.data;
  const issues: AggregateGroupsInvalidResultIssue[] = [];

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

  ensureColumnTablesAllowed(data.aggregates.count ?? undefined, "aggregates.count");
  ensureColumnTablesAllowed(
    data.aggregates.countDistinct ?? undefined,
    "aggregates.countDistinct"
  );
  ensureColumnTablesAllowed(data.aggregates.sum ?? undefined, "aggregates.sum");
  ensureColumnTablesAllowed(data.aggregates.min ?? undefined, "aggregates.min");
  ensureColumnTablesAllowed(data.aggregates.max ?? undefined, "aggregates.max");
  ensureColumnTablesAllowed(data.aggregates.avg ?? undefined, "aggregates.avg");

  if (data.having) {
    data.having.forEach((condition, index) => {
      if (condition.type === "groupKey") {
        if (!aliasSet.has(condition.key)) {
          issues.push({
            path: `having[${index}].key`,
            message: `HAVING groupKey ${condition.key} must reference a groupBy alias`,
            code: "having_groupkey_missing",
          });
        }
        return;
      }

      ensureColumnTablesAllowed(
        [condition.column],
        `having[${index}].column`
      );

      if (
        condition.function !== "count" &&
        condition.function !== "countDistinct" &&
        !ctx.numericalColumns.includes(condition.column)
      ) {
        issues.push({
          path: `having[${index}].column`,
          message: `Column ${condition.column} is not numerical for function ${condition.function}`,
          code: "having_not_numeric",
        });
      }
    });
  }

  const hasAnyMetric =
    (data.aggregates.count?.length ?? 0) > 0 ||
    (data.aggregates.countDistinct?.length ?? 0) > 0 ||
    (data.aggregates.sum?.length ?? 0) > 0 ||
    (data.aggregates.min?.length ?? 0) > 0 ||
    (data.aggregates.max?.length ?? 0) > 0 ||
    (data.aggregates.avg?.length ?? 0) > 0;

  if (!data.aggregates.groupCount && !hasAnyMetric) {
    issues.push({
      path: "aggregates",
      message:
        "Provide at least one aggregate metric or set groupCount: true.",
      code: "aggregate_required",
    });
  }

  if (data.reducers) {
    const reducerFamilies: Array<{
      key:
        | "count"
        | "countDistinct"
        | "sum"
        | "min"
        | "max"
        | "avg";
      metrics?: string[] | null;
    }> = [
      { key: "count", metrics: data.aggregates.count },
      { key: "countDistinct", metrics: data.aggregates.countDistinct },
      { key: "sum", metrics: data.aggregates.sum },
      { key: "min", metrics: data.aggregates.min },
      { key: "max", metrics: data.aggregates.max },
      { key: "avg", metrics: data.aggregates.avg },
    ];
    reducerFamilies.forEach(({ key, metrics }) => {
      const reducersForKey = data.reducers?.[key];
      if (reducersForKey && (!metrics || metrics.length === 0)) {
        issues.push({
          path: `reducers.${key}`,
          message: `Reducers provided for ${key} but no matching aggregate metrics supplied.`,
          code: "reducers_without_metrics",
        });
      }
    });
  }

  if (issues.length) {
    return { status: "invalid", issues };
  }

  const cleanAgg = (agg: string[] | null | undefined) =>
    agg && agg.length > 0 ? agg : undefined;
  const cleanHaving = (having: typeof data.having) =>
    having && having.length > 0 ? having : null;

  const cleanedAggregates: AggregateGroupsQuery["operationParameters"]["aggregates"] =
    {
      groupCount: data.aggregates.groupCount,
      ...(cleanAgg(data.aggregates.count)
        ? { count: cleanAgg(data.aggregates.count) }
        : {}),
      ...(cleanAgg(data.aggregates.countDistinct)
        ? { countDistinct: cleanAgg(data.aggregates.countDistinct) }
        : {}),
      ...(cleanAgg(data.aggregates.sum)
        ? { sum: cleanAgg(data.aggregates.sum) }
        : {}),
      ...(cleanAgg(data.aggregates.min)
        ? { min: cleanAgg(data.aggregates.min) }
        : {}),
      ...(cleanAgg(data.aggregates.max)
        ? { max: cleanAgg(data.aggregates.max) }
        : {}),
      ...(cleanAgg(data.aggregates.avg)
        ? { avg: cleanAgg(data.aggregates.avg) }
        : {}),
    };

  const reducers: AggregateGroupsQuery["operationParameters"]["reducers"] = {};
  const families: Array<{
    key:
      | "count"
      | "countDistinct"
      | "sum"
      | "min"
      | "max"
      | "avg";
    metrics: string[] | null | undefined;
    defaults?: Array<"sum" | "min" | "max" | "avg">;
  }> = [
    { key: "count", metrics: cleanedAggregates.count, defaults: ["sum"] },
    {
      key: "countDistinct",
      metrics: cleanedAggregates.countDistinct,
      defaults: ["sum"],
    },
    { key: "sum", metrics: cleanedAggregates.sum, defaults: ["sum"] },
    { key: "min", metrics: cleanedAggregates.min },
    { key: "max", metrics: cleanedAggregates.max },
    { key: "avg", metrics: cleanedAggregates.avg },
  ];

  families.forEach(({ key, metrics, defaults }) => {
    const provided = data.reducers?.[key] ?? undefined;
    if (metrics && metrics.length > 0) {
      const value = provided ?? defaults;
      if (value && value.length > 0) {
        (reducers as Record<string, string[]>)[key] = value;
      }
    }
  });

  const result: AggregateGroupsQuery["operationParameters"] = {
    joins: validatedJoins ?? null,
    groupBy: resolvedGroupBy,
    having: cleanHaving(data.having),
    aggregates: cleanedAggregates,
    reducers: Object.keys(reducers).length ? reducers : undefined,
  };

  return { status: "valid", result };
}

function validateJoins(
  joins: z.infer<typeof joinSchema>[] | null,
  ctx: AggregateGroupsValidatorContext,
  issues: AggregateGroupsInvalidResultIssue[]
): AggregateGroupsQuery["operationParameters"]["joins"] {
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
        AggregateGroupsQuery["operationParameters"]["joins"]
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
