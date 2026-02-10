import { z } from "zod";

export type SQLOperator = "+" | "-" | "*" | "/" | "%";
export type SQLFunctionName = "ABS";

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

export type SQLFunction = {
  type: "function";
  name: SQLFunctionName;
  arguments: [SQLComputedColumnAst];
};

export type SQLBrackets = {
  type: "brackets";
  expression: SQLComputedColumnAst;
};

export type SQLComputedColumnAst =
  | SQLColumnReference
  | SQLValue
  | SQLFunction
  | SQLOperation
  | SQLBrackets;

export type SQLCastExpressionAst =
  | SQLCastColumnReference
  | SQLCastValue
  | SQLCastOperation
  | SQLCastCoalesce
  | SQLCastBrackets;

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

export const SQLComputedColumnAstSchema: z.ZodType<SQLComputedColumnAst> =
  z.lazy(() =>
    z.union([
      SQLColumnReferenceSchema,
      SQLValueSchema,
      SQLFunctionSchema,
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

const SQLColumnReferenceSchema: z.ZodType<SQLColumnReference> = z.object({
  type: z.literal("column"),
  name: z.string(),
});

const SQLValueSchema: z.ZodType<SQLValue> = z.object({
  type: z.literal("value"),
  value: z.number(),
});

const SQLFunctionNameSchema: z.ZodType<SQLFunctionName> = z.union([
  z.literal("ABS"),
]);

const SQLFunctionSchema: z.ZodType<SQLFunction> = z.object({
  type: z.literal("function"),
  name: SQLFunctionNameSchema,
  arguments: z.tuple([SQLComputedColumnAstSchema]),
});

const SQLOperationSchema: z.ZodType<SQLOperation> = z.object({
  type: z.literal("operation"),
  operator: SQLOperatorSchema,
  operands: z.array(SQLComputedColumnAstSchema),
});

const SQLBracketsSchema: z.ZodType<SQLBrackets> = z.object({
  type: z.literal("brackets"),
  expression: SQLComputedColumnAstSchema,
});

const SQLCastColumnReferenceSchema: z.ZodType<SQLCastColumnReference> =
  z.object({
    type: z.literal("column"),
    name: z.string(),
  });

const SQLCastValueSchema: z.ZodType<SQLCastValue> = z.object({
  type: z.literal("value"),
  value: z.union([z.string(), z.number(), z.boolean(), z.null()]),
});

const SQLCastOperationSchema: z.ZodType<SQLCastOperation> = z.object({
  type: z.literal("cast"),
  as: z.enum(["number", "integer", "bigint", "decimal", "float"]),
  expression: z.lazy(() => SQLCastExpressionAstSchema),
});

const SQLCastCoalesceSchema: z.ZodType<SQLCastCoalesce> = z.object({
  type: z.literal("coalesce"),
  expression: z.lazy(() => SQLCastExpressionAstSchema),
  fallback: z.lazy(() => SQLCastExpressionAstSchema),
});

const SQLCastBracketsSchema: z.ZodType<SQLCastBrackets> = z.object({
  type: z.literal("brackets"),
  expression: z.lazy(() => SQLCastExpressionAstSchema),
});

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

const _computedColumnSchema = z.object({
  name: z.string(),
  table: z.object({
    name: z.string(),
  }),
  ast: SQLComputedColumnAstSchema,
});

export type ComputedColumn = z.infer<typeof _computedColumnSchema>;

const conditionOperators = z.object({
  equals: z.any().optional(),
  not: z.any().optional(),
  in: z.array(z.any()).optional(),
  notIn: z.array(z.any()).optional(),
  lt: z.any().optional(),
  lte: z.any().optional(),
  gt: z.any().optional(),
  gte: z.any().optional(),
  contains_insensitive: z.any().optional(),
  contains: z.any().optional(),
  startsWith: z.any().optional(),
  endsWith: z.any().optional(),
});

type RecursiveCondition = z.infer<typeof conditionOperators> & {
  [key: string]: any;
};

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

const formattedTableConditionsSchema = z.record(
  z.string(), // column name
  z.record(z.string(), z.union([z.string(), z.number()])), // operator and value
);

// The complete whereAndArray schema
export const whereAndArraySchema = z.array(
  z.union([formattedTableConditionsSchema, questionConditionsSchema]),
);
export type WhereConditions = z.infer<typeof whereAndArraySchema>;

// Table condition schema for row-level security / tenant filtering
const tableConditionSchema = z.object({
  column: z.string(),
  value: z.union([z.string(), z.number()]),
});

export const tableConditionsMapSchema = z
  .record(z.string(), tableConditionSchema)
  .nullable();

export type TableConditionsMap = z.infer<typeof tableConditionsMapSchema>;

const baseSchema = {
  table: z.string(),
  tableSchema: z.string().nullable().optional(),  // Database schema name (e.g., 'public', 'sales')
  whereAndArray: whereAndArraySchema,
  tableConditions: tableConditionsMapSchema,
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

const selectMapSchema = z
  .record(z.string(), z.array(z.string()).min(1))
  .refine(
    (value) => Object.keys(value).length > 0,
    "Select map must include at least one table.",
  );

const findManySchema = z
  .object({
    ...baseSchema,
    operation: z.literal("findMany"),
    operationParameters: z
      .object({
        select: selectMapSchema,
        joins: z.array(joinDescriptorSchema).nullable().optional(),
        orderBy: z
          .object({
            column: z.string(),
            direction: z.enum(["asc", "desc"]),
          })
          .strict()
          .nullable(),
        limit: z.number().int().positive(),
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
        limit: z.number().int().positive(),
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

const groupByHavingSchema = z
  .array(z.union([groupByHavingAggregateSchema, groupByHavingGroupKeySchema]))
  .nullable()
  .optional();
export type GroupByHaving = z.infer<typeof groupByHavingSchema>;

const aggregateGroupReducersSchema = z
  .array(z.enum(["sum", "min", "max", "avg"]))
  .nonempty()
  .optional();

const aggregateGroupReducersObject = z
  .object({
    count: aggregateGroupReducersSchema,
    countDistinct: aggregateGroupReducersSchema,
    sum: aggregateGroupReducersSchema,
    min: aggregateGroupReducersSchema,
    max: aggregateGroupReducersSchema,
    avg: aggregateGroupReducersSchema,
  })
  .partial()
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
        aggregates: z
          .object({
            groupCount: z.boolean().optional(),
            count: z.array(z.string()).nullable().optional(),
            countDistinct: z.array(z.string()).nullable().optional(),
            sum: z.array(z.string()).nullable().optional(),
            min: z.array(z.string()).nullable().optional(),
            max: z.array(z.string()).nullable().optional(),
            avg: z.array(z.string()).nullable().optional(),
          })
          .strict(),
        reducers: aggregateGroupReducersObject.optional(),
      })
      .strict(),
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
        limit: z.number().int().positive(),
        having: groupByHavingSchema,
      })
      .strict(),
  })
  .strict();

export const QuerySchema = z.discriminatedUnion("operation", [
  findManySchema,
  findDistinctSchema,
  findDistinctByEditDistanceSchema,
  countSchema,
  countRelationsSchema,
  aggregateSchema,
  aggregateGroupsSchema,
  groupBySchema,
]);

export type Query = z.infer<typeof QuerySchema>;
