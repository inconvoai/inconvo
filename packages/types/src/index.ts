import { z } from "zod";

export const queryResponseSchema = z
  .object({
    query: z
      .object({
        sql: z.string(),
        params: z.array(z.unknown()),
      })
      .strict(),
    data: z.unknown(),
  })
  .strict();

export type QueryResponse = z.infer<typeof queryResponseSchema>;

const columnSchema = z
  .object({
    name: z.string(),
    type: z.string(),
  })
  .strict();

const relationSchema = z
  .object({
    relationId: z.string().optional(),
    name: z.string(),
    isList: z.boolean(),
    targetTable: z.string(),
    sourceColumns: z.array(z.string()).optional(),
    targetColumns: z.array(z.string()).optional(),
  })
  .strict();

const tableSchema = z
  .object({
    name: z.string(),
    columns: z.array(columnSchema),
    relations: z.array(relationSchema).optional(),
  })
  .strict();

export const SchemaResponseSchema = z
  .object({
    tables: z.array(tableSchema),
  })
  .strict();

export type SchemaResponse = z.infer<typeof SchemaResponseSchema>;

export const InconvoOptionsSchema = z
  .object({
    baseURL: z.string().url(),
    signingSecret: z.string(),
    augmentationsHash: z.string().optional(),
  })
  .strict();

export type InconvoOptions = z.infer<typeof InconvoOptionsSchema>;

export type SQLOperator = "+" | "-" | "*" | "/" | "%";

export type SQLColumnReference = {
  type: "column";
  name: string;
};

export type SQLValue = {
  type: "value";
  value: number;
};

export type SQLOperation = {
  type: "operation";
  operator: SQLOperator;
  operands: SQLComputedColumnAst[];
};

export type SQLBrackets = {
  type: "brackets";
  expression: SQLComputedColumnAst;
};

export type SQLComputedColumnAst =
  | SQLColumnReference
  | SQLValue
  | SQLOperation
  | SQLBrackets;

export type LogicalCastType =
  | "number"
  | "integer"
  | "bigint"
  | "decimal"
  | "float";

export type SQLCastColumnReference = {
  type: "column";
  name: string;
};

export type SQLCastValue = {
  type: "value";
  value: string | number | boolean | null;
};

export type SQLCastOperation = {
  type: "cast";
  as: LogicalCastType;
  expression: SQLCastExpressionAst;
};

export type SQLCastCoalesce = {
  type: "coalesce";
  expression: SQLCastExpressionAst;
  fallback: SQLCastExpressionAst;
};

export type SQLCastBrackets = {
  type: "brackets";
  expression: SQLCastExpressionAst;
};

export type SQLCastExpressionAst =
  | SQLCastColumnReference
  | SQLCastValue
  | SQLCastOperation
  | SQLCastCoalesce
  | SQLCastBrackets;

export const SQLComputedColumnAstSchema: z.ZodType<SQLComputedColumnAst> =
  z.lazy(() =>
    z.union([
      SQLColumnReferenceSchema,
      SQLValueSchema,
      SQLOperationSchema,
      SQLBracketsSchema,
    ]),
  );

const SQLOperatorSchema: z.ZodType<SQLOperator> = z.union([
  z.literal("+"),
  z.literal("-"),
  z.literal("*"),
  z.literal("/"),
  z.literal("%"),
]);

const SQLColumnReferenceSchema: z.ZodType<SQLColumnReference> = z
  .object({
    type: z.literal("column"),
    name: z.string(),
  })
  .strict();

const SQLValueSchema: z.ZodType<SQLValue> = z
  .object({
    type: z.literal("value"),
    value: z.number(),
  })
  .strict();

const SQLOperationSchema: z.ZodType<SQLOperation> = z
  .object({
    type: z.literal("operation"),
    operator: SQLOperatorSchema,
    operands: z.array(SQLComputedColumnAstSchema),
  })
  .strict();

const SQLBracketsSchema: z.ZodType<SQLBrackets> = z
  .object({
    type: z.literal("brackets"),
    expression: SQLComputedColumnAstSchema,
  })
  .strict();

const SQLCastColumnReferenceSchema: z.ZodType<SQLCastColumnReference> = z
  .object({
    type: z.literal("column"),
    name: z.string(),
  })
  .strict();

const SQLCastValueSchema: z.ZodType<SQLCastValue> = z
  .object({
    type: z.literal("value"),
    value: z.union([z.string(), z.number(), z.boolean(), z.null()]),
  })
  .strict();

const SQLCastOperationSchema: z.ZodType<SQLCastOperation> = z
  .object({
    type: z.literal("cast"),
    as: z.enum(["number", "integer", "bigint", "decimal", "float"]),
    expression: z.lazy(() => SQLCastExpressionAstSchema),
  })
  .strict();

const SQLCastCoalesceSchema: z.ZodType<SQLCastCoalesce> = z
  .object({
    type: z.literal("coalesce"),
    expression: z.lazy(() => SQLCastExpressionAstSchema),
    fallback: z.lazy(() => SQLCastExpressionAstSchema),
  })
  .strict();

const SQLCastBracketsSchema: z.ZodType<SQLCastBrackets> = z
  .object({
    type: z.literal("brackets"),
    expression: z.lazy(() => SQLCastExpressionAstSchema),
  })
  .strict();

export const SQLCastExpressionAstSchema: z.ZodType<SQLCastExpressionAst> =
  z.lazy(() =>
    z.union([
      SQLCastColumnReferenceSchema,
      SQLCastValueSchema,
      SQLCastOperationSchema,
      SQLCastCoalesceSchema,
      SQLCastBracketsSchema,
    ]),
  );

export const computedColumnSchema = z
  .object({
    name: z.string(),
    table: z.object({
      name: z.string(),
    }),
    ast: SQLComputedColumnAstSchema,
    unit: z.string().nullable(),
    notes: z.string().nullable().optional(),
    type: z.enum(["number"]),
  })
  .strict();

// Relaxed TypeScript view: ignore the specific AST structure & enum literal restriction.
// Needed because the AST is not typed when retrieved from the database yet.
export type ComputedColumn = Omit<
  z.infer<typeof computedColumnSchema>,
  "ast" | "type"
> & {
  ast: unknown; // intentionally broad as stored as JSON in db, no type safety
  type: string; // allow any string instead of just "number"
  notes?: string | null;
};

export const tableConditionsSchema = z
  .array(
    z
      .object({
        column: z.string(),
        operator: z.enum([
          "gte",
          "lte",
          "equals",
          "not",
          "none",
          "lt",
          "gt",
          "in",
        ]),
        value: z.union([
          z.string(),
          z.number(),
          z.null(),
          z.object({}),
          z.boolean(),
          z.array(z.string()),
          z.array(z.number()),
        ]),
      })
      .strict(),
  )
  .nullable();
export type TableConditions = z.infer<typeof tableConditionsSchema>;

export const manualRelationSyncSchemaItem = z
  .object({
    name: z.string(),
    relationId: z.string().optional(),
    isList: z.boolean(),
    sourceTable: z.string(),
    targetTable: z.string(),
    sourceColumns: z.array(z.string()).min(1),
    targetColumns: z.array(z.string()).min(1),
    selected: z.boolean().optional(),
    status: z.enum(["VALID", "BROKEN"]).optional(),
    errorTag: z.string().nullable().optional(),
  })
  .strict();

export type ManualRelationSyncItem = z.infer<
  typeof manualRelationSyncSchemaItem
>;

export const manualRelationsSyncSchema = z
  .object({
    updatedAt: z.string().datetime().optional(),
    relations: z.array(manualRelationSyncSchemaItem),
  })
  .strict();

export type ManualRelationsSync = z.infer<typeof manualRelationsSyncSchema>;

export const computedColumnSyncItemSchema = z
  .object({
    name: z.string(),
    table: z.string(),
    ast: z.unknown(),
    unit: z.string().nullable().optional(),
    notes: z.string().nullable().optional(),
    type: z.string().optional(),
    selected: z.boolean().optional(),
  })
  .strict();

export type ComputedColumnSyncItem = z.infer<
  typeof computedColumnSyncItemSchema
>;

export const computedColumnsSyncSchema = z
  .object({
    updatedAt: z.string().datetime().optional(),
    computedColumns: z.array(computedColumnSyncItemSchema),
  })
  .strict();

export type ComputedColumnsSync = z.infer<typeof computedColumnsSyncSchema>;

export const columnConversionSyncItemSchema = z
  .object({
    column: z.string(),
    table: z.string(),
    ast: SQLCastExpressionAstSchema,
    type: z.string().optional(),
    selected: z.boolean().optional(),
  })
  .strict();

export type ColumnConversionSyncItem = z.infer<
  typeof columnConversionSyncItemSchema
>;

export const columnConversionsSyncSchema = z
  .object({
    updatedAt: z.string().datetime().optional(),
    columnConversions: z.array(columnConversionSyncItemSchema),
  })
  .strict();

export type ColumnConversionsSync = z.infer<typeof columnConversionsSyncSchema>;

// Unified augmentations schema - combines all three augmentation types
export const unifiedAugmentationsSyncSchema = z
  .object({
    updatedAt: z.string().datetime(),
    hash: z.string(),
    relations: z.array(manualRelationSyncSchemaItem),
    computedColumns: z.array(computedColumnSyncItemSchema),
    columnConversions: z.array(columnConversionSyncItemSchema),
  })
  .strict();

export type UnifiedAugmentationsSync = z.infer<
  typeof unifiedAugmentationsSyncSchema
>;

export const dateConditionSchema = z
  .object({
    OR: z.array(
      z
        .object({
          AND: z.array(
            z
              .object({
                column: z.string(),
                operator: z.enum(["gte", "lte", "equals"]),
                value: z.string(),
              })
              .strict(),
          ),
        })
        .strict(),
    ),
  })
  .strict()
  .nullable();
export type DateCondition = z.infer<typeof dateConditionSchema>;

const conditionOperators = z.object({
  equals: z.any().optional(),
  not: z.any().optional(),
  in: z.array(z.any()).optional(),
  notIn: z.array(z.any()).optional(),
  lt: z.any().optional(),
  lte: z.any().optional(),
  gt: z.any().optional(),
  gte: z.any().optional(),
  contains: z.any().optional(),
  contains_insensitive: z.any().optional(),
  startsWith: z.any().optional(),
  endsWith: z.any().optional(),
});

type RecursiveCondition = z.infer<typeof conditionOperators> &
  Record<string, unknown>;

const questionConditionSchema: z.ZodType<RecursiveCondition> = z.lazy(() =>
  z
    .object({
      AND: z.array(questionConditionSchema).optional(),
      OR: z.array(questionConditionSchema).optional(),
      NOT: z.array(questionConditionSchema).optional(),
    })
    .catchall(
      z.union([
        questionConditionSchema, // recursive boolean logic
        z.null(), // explicit null allowed
        conditionOperators, // scalar operators FIRST!
        z.object({
          is: questionConditionSchema.optional(),
          isNot: questionConditionSchema.optional(),
          some: questionConditionSchema.optional(),
          every: questionConditionSchema.optional(),
          none: questionConditionSchema.optional(),
        }),
      ]),
    ),
);

export const questionConditionsSchema = z
  .object({
    AND: z.array(questionConditionSchema),
  })
  .nullable();

export type QuestionConditions = z.infer<typeof questionConditionsSchema>;

const dateConditionsQuerySchema = z
  .object({
    OR: z.array(
      z
        .object({
          AND: z.array(
            z.record(
              z.string(), // column name
              z.record(z.string(), z.string()), // operator and value (always string for dates)
            ),
          ),
        })
        .strict(),
    ),
  })
  .strict();

const formattedTableConditionsSchema = z.record(
  z.string(), // column name
  z.record(z.string(), z.union([z.string(), z.number()])), // operator and value
);

// The complete whereAndArray schema
export const whereAndArraySchema = z.array(
  z.union([
    formattedTableConditionsSchema,
    questionConditionsSchema,
    dateConditionsQuerySchema,
  ]),
);
export type WhereAndArray = z.infer<typeof whereAndArraySchema>;

const JsonColumnSchemaSchema = z.array(
  z
    .object({
      tableName: z.string(),
      jsonColumnName: z.string(),
      jsonSchema: z.array(
        z
          .object({
            name: z.string(),
            relation: z.null(),
            type: z.string(),
          })
          .strict(),
      ),
    })
    .strict(),
);

export type JsonColumnSchema = z.infer<typeof JsonColumnSchemaSchema>;

const baseSchema = {
  table: z.string(),
  whereAndArray: whereAndArraySchema,
  jsonColumnSchema: JsonColumnSchemaSchema.optional(),
};

const fullyQualifiedColumnSchema = z
  .string()
  .min(3)
  .refine(
    (value) => value.includes("."),
    "Join column references must be fully qualified (table.column).",
  );

export const joinPathHopSchema = z
  .object({
    source: z.array(fullyQualifiedColumnSchema).min(1),
    target: z.array(fullyQualifiedColumnSchema).min(1),
  })
  .strict()
  .superRefine((hop, ctx) => {
    if (hop.source.length !== hop.target.length) {
      ctx.addIssue({
        code: z.ZodIssueCode.custom,
        message:
          "Join path hop must pair equal numbers of source and target columns.",
      });
    }
  });

export const joinPathSchema = z.array(joinPathHopSchema).min(1);
export type JoinPathHop = z.infer<typeof joinPathHopSchema>;

export const joinTypeSchema = z.enum(["inner", "left", "right"]);

export const joinDescriptorSchema = z
  .object({
    table: z.string(),
    name: z.string().min(1).optional(),
    path: joinPathSchema,
    joinType: joinTypeSchema.optional(),
  })
  .strict();

export const findManySchema = z
  .object({
    ...baseSchema,
    operation: z.literal("findMany"),
    operationParameters: z
      .object({
        select: z.record(z.string(), z.array(z.string())),
        joins: z.array(joinDescriptorSchema).nullable().optional(),
        orderBy: z
          .object({
            column: z.string(),
            direction: z.enum(["asc", "desc"]),
          })
          .strict()
          .nullable(),
        limit: z.number(),
      })
      .strict(),
  })
  .strict();

const findDistinctSchema = z
  .object({
    ...baseSchema,
    operation: z.literal("findDistinct"),
    operationParameters: z
      .object({
        column: z.string(),
      })
      .strict(),
  })
  .strict();

const findDistinctByEditDistanceSchema = z
  .object({
    ...baseSchema,
    operation: z.literal("findDistinctByEditDistance"),
    operationParameters: z
      .object({
        column: z.string(),
        compareString: z.string(),
      })
      .strict(),
  })
  .strict();

const countMetricsArraySchema = z.array(z.string()).min(1);

const countSchema = z
  .object({
    ...baseSchema,
    operation: z.literal("count"),
    operationParameters: z
      .object({
        joins: z.array(joinDescriptorSchema).nullable().optional(),
        count: countMetricsArraySchema.nullable().optional().default(null),
        countDistinct: countMetricsArraySchema
          .nullable()
          .optional()
          .default(null),
      })
      .strict()
      .superRefine((params, ctx) => {
        const hasCount = Array.isArray(params.count) && params.count.length > 0;
        const hasDistinct =
          Array.isArray(params.countDistinct) &&
          params.countDistinct.length > 0;

        if (!hasCount && !hasDistinct) {
          ctx.addIssue({
            code: z.ZodIssueCode.custom,
            message:
              "Provide at least one metric via `count` or `countDistinct`.",
            path: ["count"],
          });
          ctx.addIssue({
            code: z.ZodIssueCode.custom,
            message:
              "Provide at least one metric via `count` or `countDistinct`.",
            path: ["countDistinct"],
          });
        }
      }),
  })
  .strict();

const countRelationsSchema = z
  .object({
    ...baseSchema,
    operation: z.literal("countRelations"),
    operationParameters: z
      .object({
        columns: z.array(z.string()),
        joins: z
          .array(
            joinDescriptorSchema.extend({
              name: z.string().min(1),
            }),
          )
          .nullable()
          .optional(),
        relationsToCount: z.array(
          z.object({
            name: z.string(),
            distinct: z.string().nullable(),
          }),
        ),
        orderBy: z
          .object({
            name: z.string(),
            direction: z.enum(["asc", "desc"]),
          })
          .strict()
          .nullable(),
        limit: z.number(),
      })
      .strict(),
  })
  .strict();

const aggregateSchema = z
  .object({
    ...baseSchema,
    operation: z.literal("aggregate"),
    operationParameters: z
      .object({
        joins: z.array(joinDescriptorSchema).nullable().optional(),
        min: z.array(z.string()).nullable(),
        max: z.array(z.string()).nullable(),
        avg: z.array(z.string()).nullable(),
        sum: z.array(z.string()).nullable(),
        count: z.array(z.string()).nullable(),
        countDistinct: z.array(z.string()).nullable(),
        median: z.array(z.string()).nullable(),
      })
      .strict(),
  })
  .strict();

const groupByColumnKeySchema = z
  .object({
    type: z.literal("column"),
    column: z.string(),
    alias: z.string().min(1).optional(),
  })
  .strict();

const groupByDateIntervalKeySchema = z
  .object({
    type: z.literal("dateInterval"),
    column: z.string(),
    interval: z.enum(["day", "week", "month", "quarter", "year", "hour"]),
    alias: z.string().min(1).optional(),
  })
  .strict();

const groupByDateComponentKeySchema = z
  .object({
    type: z.literal("dateComponent"),
    column: z.string(),
    component: z.enum(["dayOfWeek", "monthOfYear", "quarterOfYear"]),
    alias: z.string().min(1).optional(),
  })
  .strict();

export const groupByKeySchema = z.union([
  groupByColumnKeySchema,
  groupByDateIntervalKeySchema,
  groupByDateComponentKeySchema,
]);
export type GroupByKey = z.infer<typeof groupByKeySchema>;

const groupByOrderBySchema = z.union([
  z
    .object({
      type: z.literal("aggregate"),
      function: z.enum(["count", "countDistinct", "sum", "min", "max", "avg"]),
      column: z.string(),
      direction: z.enum(["asc", "desc"]),
    })
    .strict(),
  z
    .object({
      type: z.literal("groupKey"),
      key: z.string(),
      direction: z.enum(["asc", "desc"]),
    })
    .strict(),
]);
export type GroupByOrderBy = z.infer<typeof groupByOrderBySchema>;

const groupByHavingOperatorSchema = z.enum([
  "equals",
  "not",
  "in",
  "notIn",
  "lt",
  "lte",
  "gt",
  "gte",
]);

const groupByHavingAggregateSchema = z
  .object({
    type: z.literal("aggregate"),
    function: z.enum(["count", "countDistinct", "sum", "min", "max", "avg"]),
    column: z.string(),
    operator: groupByHavingOperatorSchema,
    value: z.any(),
  })
  .strict();

const groupByHavingGroupKeySchema = z
  .object({
    type: z.literal("groupKey"),
    key: z.string(),
    operator: groupByHavingOperatorSchema,
    value: z.any(),
  })
  .strict();

export const groupByHavingSchema = z
  .array(z.union([groupByHavingAggregateSchema, groupByHavingGroupKeySchema]))
  .nullable()
  .optional();
export type GroupByHaving = z.infer<typeof groupByHavingSchema>;

const reducerSchema = z.enum(["sum", "min", "max", "avg"]);

const aggregateGroupsReducersSchema = z
  .object({
    count: z.array(reducerSchema).min(1).nullable().optional(),
    countDistinct: z.array(reducerSchema).min(1).nullable().optional(),
    sum: z.array(reducerSchema).min(1).nullable().optional(),
    min: z.array(reducerSchema).min(1).nullable().optional(),
    max: z.array(reducerSchema).min(1).nullable().optional(),
    avg: z.array(reducerSchema).min(1).nullable().optional(),
  })
  .strict()
  .optional();

const aggregateGroupsAggregatesSchema = z
  .object({
    groupCount: z.boolean().optional(),
    count: z.array(z.string()).nullable().optional(),
    countDistinct: z.array(z.string()).nullable().optional(),
    sum: z.array(z.string()).nullable().optional(),
    min: z.array(z.string()).nullable().optional(),
    max: z.array(z.string()).nullable().optional(),
    avg: z.array(z.string()).nullable().optional(),
  })
  .strict();

const aggregateGroupsSchema = z
  .object({
    ...baseSchema,
    operation: z.literal("aggregateGroups"),
    operationParameters: z
      .object({
        joins: z.array(joinDescriptorSchema).nullable().optional(),
        groupBy: z.array(groupByKeySchema).min(1),
        having: groupByHavingSchema,
        aggregates: aggregateGroupsAggregatesSchema,
        reducers: aggregateGroupsReducersSchema,
      })
      .strict()
      .superRefine((params, ctx) => {
        const agg = params.aggregates;
        const hasColumns =
          (agg.count?.length ?? 0) > 0 ||
          (agg.countDistinct?.length ?? 0) > 0 ||
          (agg.sum?.length ?? 0) > 0 ||
          (agg.min?.length ?? 0) > 0 ||
          (agg.max?.length ?? 0) > 0 ||
          (agg.avg?.length ?? 0) > 0;
        if (!agg.groupCount && !hasColumns) {
          ctx.addIssue({
            code: z.ZodIssueCode.custom,
            message:
              "Provide at least one aggregate metric or set groupCount: true.",
            path: ["aggregates"],
          });
        }

        const reducers = params.reducers ?? {};
        const families: Array<{
          key: "count" | "countDistinct" | "sum" | "min" | "max" | "avg";
          values?: string[] | null;
          metrics?: string[] | null;
        }> = [
          { key: "count", values: reducers.count ?? null, metrics: agg.count },
          {
            key: "countDistinct",
            values: reducers.countDistinct ?? null,
            metrics: agg.countDistinct,
          },
          { key: "sum", values: reducers.sum ?? null, metrics: agg.sum },
          { key: "min", values: reducers.min ?? null, metrics: agg.min },
          { key: "max", values: reducers.max ?? null, metrics: agg.max },
          { key: "avg", values: reducers.avg ?? null, metrics: agg.avg },
        ];

        families.forEach(({ key, values, metrics }) => {
          if (values && (!metrics || metrics.length === 0)) {
            ctx.addIssue({
              code: z.ZodIssueCode.custom,
              message: `Reducers provided for ${key} but no matching aggregate metrics supplied.`,
              path: ["reducers", key],
            });
          }
        });
      }),
  })
  .strict();

const groupBySchema = z
  .object({
    ...baseSchema,
    operation: z.literal("groupBy"),
    operationParameters: z
      .object({
        joins: z.array(joinDescriptorSchema).nullable().optional(),
        groupBy: z.array(groupByKeySchema).min(1),
        count: z.array(z.string()).nullable(),
        countDistinct: z.array(z.string()).nullable(),
        sum: z.array(z.string()).nullable(),
        min: z.array(z.string()).nullable(),
        max: z.array(z.string()).nullable(),
        avg: z.array(z.string()).nullable(),
        orderBy: groupByOrderBySchema,
        having: groupByHavingSchema,
        limit: z.number(),
      })
      .strict(),
  })
  .strict();

export const querySchema = z.discriminatedUnion("operation", [
  findManySchema,
  findDistinctSchema,
  findDistinctByEditDistanceSchema,
  countSchema,
  countRelationsSchema,
  aggregateSchema,
  aggregateGroupsSchema,
  groupBySchema,
]);

export type Query = z.infer<typeof querySchema>;
export type FindManyQuery = z.infer<typeof findManySchema>;
export type FindDistinctQuery = z.infer<typeof findDistinctSchema>;
export type FindDistinctByEditDistanceQuery = z.infer<
  typeof findDistinctByEditDistanceSchema
>;
export type CountQuery = z.infer<typeof countSchema>;
export type CountRelationsQuery = z.infer<typeof countRelationsSchema>;
export type AggregateQuery = z.infer<typeof aggregateSchema>;
export type AggregateGroupsQuery = z.infer<typeof aggregateGroupsSchema>;
export type GroupByQuery = z.infer<typeof groupBySchema>;

// Base schemas for responses/messages
const baseTextSchema = z.object({
  type: z.literal("text"),
  message: z.string().describe("The message to display."),
});

const baseErrorSchema = z.object({
  type: z.literal("error"),
  message: z.string().describe("The message to display."),
});

const baseTableSchema = z.object({
  type: z.literal("table"),
  message: z.string().describe("The message to display."),
  table: z
    .object({
      head: z.array(z.string()).describe("The table column names"),
      body: z
        .array(z.array(z.string()))
        .describe(
          "The table rows, each row is an array of strings, the number of items must match the head array length",
        ),
    })
    .strict(),
});

// Vega-Lite v5 spec schema for rich visualizations
// Uses passthrough() to allow the full Vega-Lite spec while validating core structure
export const vegaLiteSpecSchema = z
  .object({
    $schema: z
      .string()
      .optional()
      .describe(
        "Vega-Lite schema URL, e.g. https://vega.github.io/schema/vega-lite/v5.json",
      ),
    title: z.union([z.string(), z.object({}).passthrough()]).optional(),
    description: z.string().optional(),
    width: z.union([z.number(), z.literal("container")]).optional(),
    height: z.union([z.number(), z.literal("container")]).optional(),
    data: z
      .object({
        values: z
          .array(z.record(z.string(), z.unknown()))
          .describe("Inline data as array of objects"),
      })
      .describe(
        "Data source with inline values only; URL-based or named data sources are not supported",
      ),
    mark: z
      .union([
        z.enum([
          "bar",
          "line",
          "point",
          "area",
          "arc",
          "rect",
          "rule",
          "text",
          "tick",
          "circle",
          "square",
          "geoshape",
        ]),
        z.object({ type: z.string() }).passthrough(),
      ])
      .describe("Mark type for the visualization"),
    encoding: z
      .object({
        x: z.object({}).passthrough().optional(),
        y: z.object({}).passthrough().optional(),
        color: z.object({}).passthrough().optional(),
        size: z.object({}).passthrough().optional(),
        shape: z.object({}).passthrough().optional(),
        opacity: z.object({}).passthrough().optional(),
        theta: z.object({}).passthrough().optional(),
        radius: z.object({}).passthrough().optional(),
        text: z.object({}).passthrough().optional(),
        tooltip: z
          .union([
            z.object({}).passthrough(),
            z.array(z.object({}).passthrough()),
          ])
          .optional(),
      })
      .passthrough()
      .optional()
      .describe("Visual encoding channels"),
    layer: z.array(z.object({}).passthrough()).optional(),
    config: z.object({}).passthrough().optional(),
  })
  .passthrough();

export type VegaLiteSpec = z.infer<typeof vegaLiteSpecSchema>;

const baseChartSchema = z.object({
  type: z.literal("chart"),
  message: z
    .string()
    .describe("Explanation of the visualization highlighting key insights."),
  spec: vegaLiteSpecSchema.describe(
    "Complete Vega-Lite v5 specification for the visualization.",
  ),
});

// Base schema for messages (no author field, no id)
const baseMessageSchema = z.discriminatedUnion("type", [
  baseTextSchema,
  baseErrorSchema,
  baseTableSchema,
  baseChartSchema,
]);

// InconvoMessage: Can be from user (no id) or Inconvo (has id)
export const inconvoMessageSchema = baseMessageSchema;
export type InconvoMessage = z.infer<typeof baseMessageSchema> & {
  id?: string;
};

// InconvoResponse: Always from Inconvo, always has an id
export const inconvoResponseSchema = baseMessageSchema;
export type InconvoResponse = z.infer<typeof baseMessageSchema> & {
  id: string;
};

export const inconvoEvalRunExample = z
  .object({
    id: z.string(),
    input: z.array(inconvoResponseSchema),
    referenceOutput: inconvoResponseSchema,
    actualOutput: inconvoResponseSchema,
    correct: z.boolean(),
  })
  .strict();

export type InconvoEvalRunExample = z.infer<typeof inconvoEvalRunExample>;

export const inconvoRequestContextSchema = z.record(
  z.string(),
  z.union([z.string(), z.number()]),
);

export type InconvoRequestContext = z.infer<typeof inconvoRequestContextSchema>;

export const inconvoExampleStatusSchema = z.enum(["PASS", "FAIL", "REVIEW"]);

export const inconvoExampleSchema = z
  .object({
    id: z.string(),
    trace_id: z.string(),
    createdAt: z.string(),
    modifiedAt: z.string().optional(),
    requestContext: inconvoRequestContextSchema,
    question: z.string(),
    answer: inconvoResponseSchema.optional(),
    status: inconvoExampleStatusSchema,
  })
  .strict();

export type InconvoExample = z.infer<typeof inconvoExampleSchema>;

export const inconvoEvalRunSchema = z
  .object({
    id: z.string(),
    name: z.string(),
    startTime: z.string(),
    endTime: z.string(),
    numExamples: z.number(),
    score: z.number().nullable(),
  })
  .strict();

export type InconvoEvalRun = z.infer<typeof inconvoEvalRunSchema>;

// Schema types for semantic model (standalone versions without Prisma dependency)
export type TableAccess = "QUERYABLE" | "JOINABLE" | "OFF";
export type RelationSource = "FK" | "MANUAL";
export type RelationStatus = "VALID" | "BROKEN";

type ColumnRelationEntry = {
  relation: {
    targetTable: { name: string | null };
  };
  targetColumn: {
    name: string | null;
  };
};

export type SchemaColumnConversion = {
  id: string;
  ast: unknown;
  type: string | null;
  selected: boolean;
};

export type SchemaColumn = {
  dbName: string;
  name: string;
  rename: string | null;
  notes: string | null;
  type: string;
  effectiveType: string;
  conversion: SchemaColumnConversion | null;
  unit: string | null;
  relation: ColumnRelationEntry[];
};

export type SchemaComputedColumn = {
  name: string;
  table: { name: string };
  type: string;
  ast: unknown;
  unit: string | null;
  notes: string | null;
};

export type SchemaRelation = {
  name: string;
  relationId: string | null;
  targetTable: { name: string };
  isList: boolean;
  selected: boolean;
  source: RelationSource;
  status: RelationStatus;
  errorTag: string | null;
  sourceColumns: string[];
  targetColumns: string[];
};

export type SchemaTable = {
  name: string;
  access: TableAccess;
  context: string | null;
  columns: SchemaColumn[];
  computedColumns: SchemaComputedColumn[];
  outwardRelations: SchemaRelation[];
  condition: {
    column: { name: string };
    requestContextField: { key: string };
  } | null;
};

export type Schema = SchemaTable[];

// Minimal Conversation type for agents (subset of Prisma Conversation)
export type Conversation = {
  id: string;
  title: string | null;
  requestContext: Record<string, string | number> | null;
};

// Database connector interface - implemented by platform's UserDatabaseConnector
// and dev-server's direct connector
export interface DatabaseConnector {
  query(query: Query): Promise<QueryResponse>;
}
