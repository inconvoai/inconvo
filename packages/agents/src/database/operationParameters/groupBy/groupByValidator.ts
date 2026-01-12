import { z } from "zod";
import {
  joinDescriptorSchema,
  joinPathHopSchema,
  joinTypeSchema,
  type GroupByKey,
  type GroupByQuery,
  type JoinPathHop,
} from "@repo/types";
import { stringArrayToZodEnum } from "../../../utils/zodHelpers";

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

  const buildAggregateArraySchema = (enumSchema: z.ZodType<string> | null) =>
    enumSchema ? z.array(enumSchema).min(1).nullable() : z.null();

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
      function: z.enum(["count", "countDistinct", "sum", "min", "max", "avg"]),
      column: allColumnsEnumOrNever,
      direction: z.enum(["asc", "desc"]),
    })
    .strict();

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

  return z.object({
    joins: z.array(joinSchema).nullable().optional(),
    groupBy: z.array(groupByKeySchema).min(1),
    count: buildAggregateArraySchema(allColumnsEnum),
    countDistinct: buildAggregateArraySchema(allColumnsEnum),
    sum: buildAggregateArraySchema(numericalColumnsEnum),
    min: buildAggregateArraySchema(numericalColumnsEnum),
    max: buildAggregateArraySchema(numericalColumnsEnum),
    avg: buildAggregateArraySchema(numericalColumnsEnum),
    orderBy: z.union([groupKeyOrderSchema, aggregateOrderSchema]),
    limit: z.number().int().positive().max(1000),
    having: havingSchema,
  });
}

const DATE_COMPONENT_RANGES: Record<
  "dayOfWeek" | "monthOfYear" | "quarterOfYear",
  { min: number; max: number; label: string }
> = {
  dayOfWeek: { min: 1, max: 7, label: "day of week (1-7)" },
  monthOfYear: { min: 1, max: 12, label: "month (1-12)" },
  quarterOfYear: { min: 1, max: 4, label: "quarter (1-4)" },
};

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

export function validateGroupByCandidate(
  candidate: unknown,
  ctx: GroupByValidatorContext,
): GroupByValidationResult {
  const schema = buildGroupByZodSchema(ctx);
  const parsed = schema.safeParse(candidate);
  if (!parsed.success) {
    const issues: GroupByInvalidResultIssue[] = parsed.error.issues.map(
      (issue) => ({
        path: issue.path.join(".") || "<root>",
        message: issue.message,
        code: issue.code,
      }),
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

  ensureColumnTablesAllowed(data.count ?? undefined, "count");
  ensureColumnTablesAllowed(data.countDistinct ?? undefined, "countDistinct");
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
      data.orderBy.function !== "countDistinct" &&
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
      "count" | "countDistinct" | "sum" | "min" | "max" | "avg",
      string[] | null | undefined
    > = {
      count: data.count,
      countDistinct: data.countDistinct,
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
          }
        }
        return;
      }

      ensureColumnTablesAllowed([condition.column], `having[${index}].column`);

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

  if (issues.length) {
    return { status: "invalid", issues };
  }

  const cleanAgg = (agg: string[] | null | undefined) =>
    agg && agg.length > 0 ? agg : null;
  const cleanHaving = (having: typeof data.having) =>
    having && having.length > 0 ? having : null;

  const result: GroupByQuery["operationParameters"] = {
    joins: validatedJoins ?? null,
    groupBy: resolvedGroupBy,
    count: cleanAgg(data.count),
    countDistinct: cleanAgg(data.countDistinct),
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
    having: cleanHaving(data.having),
    limit: data.limit,
  };

  return { status: "valid", result };
}

function validateJoins(
  joins: z.infer<typeof joinSchema>[] | null,
  ctx: GroupByValidatorContext,
  issues: GroupByInvalidResultIssue[],
): GroupByQuery["operationParameters"]["joins"] {
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
