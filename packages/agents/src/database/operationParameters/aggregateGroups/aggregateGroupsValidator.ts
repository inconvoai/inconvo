import { z } from "zod";
import {
  joinDescriptorSchema,
  joinPathHopSchema,
  joinTypeSchema,
  type AggregateGroupsQuery,
  type GroupByKey,
  type JoinPathHop,
} from "@repo/types";
import { stringArrayToZodEnum } from "../../../utils/zodHelpers";

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

const DATE_COMPONENT_RANGES: Record<
  "dayOfWeek" | "monthOfYear" | "quarterOfYear",
  { min: number; max: number; label: string }
> = {
  dayOfWeek: { min: 1, max: 7, label: "day of week (1-7)" },
  monthOfYear: { min: 1, max: 12, label: "month (1-12)" },
  quarterOfYear: { min: 1, max: 4, label: "quarter (1-4)" },
};

/**
 * Bucket format patterns for dateInterval grouping
 * These match the output format from buildDateIntervalExpression
 */
const DATE_INTERVAL_PATTERNS: Record<
  "day" | "week" | "month" | "quarter" | "year" | "hour",
  { regex: RegExp; example: string }
> = {
  day: { regex: /^\d{4}-\d{2}-\d{2}$/, example: "2024-01-15" },
  week: { regex: /^\d{4}-W?\d{1,2}$/, example: "2024-W03 or 202403" },
  month: { regex: /^\d{4}-\d{2}$/, example: "2024-01" },
  quarter: { regex: /^\d{4}-Q[1-4]$/, example: "2024-Q1" },
  year: { regex: /^\d{4}$/, example: "2024" },
  hour: { regex: /^\d{4}-\d{2}-\d{2} \d{2}:00$/, example: "2024-01-15 14:00" },
};

/**
 * Check if a string matches the expected bucket format for a dateInterval
 */
function isValidDateIntervalBucket(
  value: string,
  interval: "day" | "week" | "month" | "quarter" | "year" | "hour",
): boolean {
  const pattern = DATE_INTERVAL_PATTERNS[interval];
  return pattern.regex.test(value);
}

/**
 * Validate that a HAVING value is appropriate for a dateInterval groupKey
 */
function validateDateIntervalHavingValue(
  value: unknown,
  operator: string,
  interval: "day" | "week" | "month" | "quarter" | "year" | "hour",
): { valid: boolean; message: string; transformedValue?: unknown } {
  const pattern = DATE_INTERVAL_PATTERNS[interval];

  const validateSingleValue = (
    v: unknown,
  ): { valid: boolean; transformed?: string } => {
    if (typeof v === "string" && isValidDateIntervalBucket(v, interval)) {
      return { valid: true, transformed: v };
    }
    // Also accept numbers for year interval (MySQL/MSSQL return numbers)
    if (interval === "year" && typeof v === "number") {
      return { valid: true, transformed: String(v) };
    }
    return { valid: false };
  };

  if (operator === "in" || operator === "notIn") {
    if (!Array.isArray(value)) {
      return {
        valid: false,
        message: `Value for '${operator}' operator must be an array`,
      };
    }
    const results = value.map((v) => validateSingleValue(v));
    const invalidIndices = results
      .map((r, i) => (!r.valid ? i : -1))
      .filter((i) => i !== -1);
    if (invalidIndices.length > 0) {
      return {
        valid: false,
        message: `Invalid ${interval} bucket values at indices ${invalidIndices.join(", ")}. Expected format: "${pattern.example}". Do not use relative expressions like "now()-30d".`,
      };
    }
    return {
      valid: true,
      message: "",
      transformedValue: results.map((r) => r.transformed),
    };
  }

  // For equals, not, lt, lte, gt, gte operators
  const result = validateSingleValue(value);
  if (!result.valid) {
    return {
      valid: false,
      message: `Invalid ${interval} bucket value: ${JSON.stringify(value)}. Expected format: "${pattern.example}". Do not use relative expressions like "now()-30d".`,
    };
  }

  return { valid: true, message: "", transformedValue: result.transformed };
}

/**
 * Check if a string is a valid ISO date string (for aggregate min/max on date columns)
 */
function isValidIsoDateString(value: string): boolean {
  // ISO 8601 format: YYYY-MM-DD or YYYY-MM-DDTHH:mm:ss.sssZ
  const isoDateRegex = /^\d{4}-\d{2}-\d{2}(T\d{2}:\d{2}:\d{2}(\.\d{3})?Z?)?$/;
  if (isoDateRegex.test(value)) {
    const date = new Date(value);
    return !isNaN(date.getTime());
  }
  return false;
}

/**
 * Validate that a HAVING value is appropriate for an aggregate on a date column (min/max)
 */
function validateDateAggregateHavingValue(
  value: unknown,
  operator: string,
): { valid: boolean; message: string; transformedValue?: unknown } {
  const validateSingleValue = (
    v: unknown,
  ): { valid: boolean; transformed?: string } => {
    if (typeof v === "string" && isValidIsoDateString(v)) {
      return { valid: true, transformed: v };
    }
    return { valid: false };
  };

  if (operator === "in" || operator === "notIn") {
    if (!Array.isArray(value)) {
      return {
        valid: false,
        message: `Value for '${operator}' operator must be an array`,
      };
    }
    const results = value.map((v) => validateSingleValue(v));
    const invalidIndices = results
      .map((r, i) => (!r.valid ? i : -1))
      .filter((i) => i !== -1);
    if (invalidIndices.length > 0) {
      return {
        valid: false,
        message: `Invalid date values at indices ${invalidIndices.join(", ")}. Expected ISO date strings (e.g., "2024-01-15" or "2024-01-15T00:00:00Z"). Do not use relative expressions like "now()-30d".`,
      };
    }
    return {
      valid: true,
      message: "",
      transformedValue: results.map((r) => r.transformed),
    };
  }

  // For equals, not, lt, lte, gt, gte operators
  const result = validateSingleValue(value);
  if (!result.valid) {
    return {
      valid: false,
      message: `Invalid date value: ${JSON.stringify(value)}. Expected ISO date string (e.g., "2024-01-15" or "2024-01-15T00:00:00Z"). Do not use relative expressions like "now()-30d".`,
    };
  }

  return { valid: true, message: "", transformedValue: result.transformed };
}

function validateDateComponentHavingValue(
  value: unknown,
  component: "dayOfWeek" | "monthOfYear" | "quarterOfYear",
  operator: string,
): { valid: boolean; message: string } {
  const range = DATE_COMPONENT_RANGES[component];

  const validateSingleValue = (v: unknown): boolean => {
    if (typeof v === "number" && Number.isInteger(v)) {
      return v >= range.min && v <= range.max;
    }
    // Also accept numeric strings
    if (typeof v === "string") {
      const parsed = parseInt(v, 10);
      if (!isNaN(parsed) && String(parsed) === v.trim()) {
        return parsed >= range.min && parsed <= range.max;
      }
    }
    return false;
  };

  if (operator === "in" || operator === "notIn") {
    if (!Array.isArray(value)) {
      return {
        valid: false,
        message: `Value for '${operator}' operator must be an array`,
      };
    }
    const invalidValues = value.filter((v) => !validateSingleValue(v));
    if (invalidValues.length > 0) {
      return {
        valid: false,
        message: `Invalid ${range.label} values: ${JSON.stringify(invalidValues)}. Expected integers from ${range.min} to ${range.max}.`,
      };
    }
    return { valid: true, message: "" };
  }

  // For equals, not, lt, lte, gt, gte operators
  if (!validateSingleValue(value)) {
    return {
      valid: false,
      message: `Invalid ${range.label} value: ${JSON.stringify(value)}. Expected integer from ${range.min} to ${range.max}.`,
    };
  }

  return { valid: true, message: "" };
}

const joinSchema = joinDescriptorSchema
  .extend({
    path: z.array(joinPathHopSchema).min(1),
    joinType: joinTypeSchema.optional(),
  })
  .strip();

export function buildAggregateGroupsZodSchema(
  ctx: AggregateGroupsValidatorContext,
) {
  const buildEnum = (values: string[]) =>
    values.length ? stringArrayToZodEnum(values) : null;

  const allColumnsEnum = buildEnum(ctx.allColumns);
  const numericalColumnsEnum = buildEnum(ctx.numericalColumns);
  const groupableColumnsEnum = buildEnum(ctx.groupableColumns);
  const intervalColumnsEnum = buildEnum(ctx.intervalColumns);

  const buildAggregateArraySchema = (enumSchema: z.ZodType<string> | null) =>
    enumSchema ? z.array(enumSchema).nullable() : z.null();

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
    columnEnum: z.ZodType<string>,
  ): z.ZodType<ColumnGroupKey> =>
    z
      .object({
        type: z.literal("column"),
        column: columnEnum,
        alias: z.string().min(1).optional(),
      })
      .strict();

  const buildIntervalGroupKeySchema = (
    columnEnum: z.ZodType<string>,
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
    columnEnum: z.ZodType<string>,
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
      keySchemas as [z.ZodTypeAny, z.ZodTypeAny, ...z.ZodTypeAny[]],
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
  ctx: AggregateGroupsValidatorContext,
): AggregateGroupsValidationResult {
  const schema = buildAggregateGroupsZodSchema(ctx);
  const parsed = schema.safeParse(candidate);
  if (!parsed.success) {
    const issues: AggregateGroupsInvalidResultIssue[] = parsed.error.issues.map(
      (issue) => ({
        path: issue.path.join(".") || "<root>",
        message: issue.message,
        code: issue.code,
      }),
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
    path: string,
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

  ensureColumnTablesAllowed(
    data.aggregates.count ?? undefined,
    "aggregates.count",
  );
  ensureColumnTablesAllowed(
    data.aggregates.countDistinct ?? undefined,
    "aggregates.countDistinct",
  );
  ensureColumnTablesAllowed(data.aggregates.sum ?? undefined, "aggregates.sum");
  ensureColumnTablesAllowed(data.aggregates.min ?? undefined, "aggregates.min");
  ensureColumnTablesAllowed(data.aggregates.max ?? undefined, "aggregates.max");
  ensureColumnTablesAllowed(data.aggregates.avg ?? undefined, "aggregates.avg");

  // Track transformed HAVING values for date columns
  const havingTransforms = new Map<number, unknown>();

  if (data.having) {
    data.having.forEach((condition, index) => {
      if (condition.type === "groupKey") {
        if (!aliasSet.has(condition.key)) {
          issues.push({
            path: `having[${index}].key`,
            message: `HAVING groupKey ${condition.key} must reference a groupBy alias`,
            code: "having_groupkey_missing",
          });
        } else {
          // Validate that the value is appropriate for the groupBy type
          const groupByEntry = resolvedGroupBy.find(
            (g) => g.alias === condition.key,
          );
          if (groupByEntry?.type === "dateComponent") {
            const valueValidation = validateDateComponentHavingValue(
              condition.value,
              groupByEntry.component,
              condition.operator,
            );
            if (!valueValidation.valid) {
              issues.push({
                path: `having[${index}].value`,
                message: valueValidation.message,
                code: "invalid_date_component_value",
              });
            }
          } else if (groupByEntry?.type === "dateInterval") {
            // For dateInterval group keys, validate bucket format values
            const valueValidation = validateDateIntervalHavingValue(
              condition.value,
              condition.operator,
              groupByEntry.interval,
            );
            if (!valueValidation.valid) {
              issues.push({
                path: `having[${index}].value`,
                message: valueValidation.message,
                code: "invalid_date_interval_value",
              });
            } else if (valueValidation.transformedValue !== undefined) {
              havingTransforms.set(index, valueValidation.transformedValue);
            }
          }
        }
        return;
      }

      ensureColumnTablesAllowed([condition.column], `having[${index}].column`);

      // Check if the column is a date column (for min/max on dates)
      const isDateColumn = ctx.intervalColumns.includes(condition.column);

      if (isDateColumn) {
        // For date columns with min/max aggregates, validate the value is an ISO date
        if (
          condition.function === "min" ||
          condition.function === "max"
        ) {
          const valueValidation = validateDateAggregateHavingValue(
            condition.value,
            condition.operator,
          );
          if (!valueValidation.valid) {
            issues.push({
              path: `having[${index}].value`,
              message: valueValidation.message,
              code: "invalid_date_value",
            });
          } else if (valueValidation.transformedValue !== undefined) {
            havingTransforms.set(index, valueValidation.transformedValue);
          }
        }
      } else if (
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
      message: "Provide at least one aggregate metric or set groupCount: true.",
      code: "aggregate_required",
    });
  }

  if (data.reducers) {
    const reducerFamilies: Array<{
      key: "count" | "countDistinct" | "sum" | "min" | "max" | "avg";
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
  const cleanHaving = (having: typeof data.having) => {
    if (!having || having.length === 0) return null;
    // Apply any transformed values from date validation
    return having.map((condition, index) => {
      const transformedValue = havingTransforms.get(index);
      if (transformedValue !== undefined) {
        return { ...condition, value: transformedValue };
      }
      return condition;
    });
  };

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
    key: "count" | "countDistinct" | "sum" | "min" | "max" | "avg";
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
  issues: AggregateGroupsInvalidResultIssue[],
): AggregateGroupsQuery["operationParameters"]["joins"] {
  if (!joins || joins.length === 0) {
    return undefined;
  }

  const joinOptionsByKey = new Map(
    ctx.joinOptions.map((option) => [joinPathKey(option.path), option]),
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
