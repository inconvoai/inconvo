import { z } from "zod";

const QueryConditionSchema = z.record(
  z.record(
    z.union([z.string(), z.number(), z.null(), z.object({}), z.boolean()])
  )
);

const QueryDateConditionSchema = z
  .object({
    OR: z.array(
      z
        .object({
          AND: z.array(
            z.record(z.record(z.union([z.string(), z.number(), z.null()])))
          ),
        })
        .strict()
    ),
  })
  .strict();

const QueryWhereAndArraySchema = z.array(
  z.union([QueryConditionSchema, QueryDateConditionSchema])
);
export type WhereConditions = z.infer<typeof QueryWhereAndArraySchema>;

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

const SQLComputedColumnAstSchema: z.ZodType<SQLComputedColumnAst> = z.lazy(() =>
  z.union([
    SQLColumnReferenceSchema,
    SQLValueSchema,
    SQLOperationSchema,
    SQLBracketsSchema,
  ])
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

const SQLOperationSchema: z.ZodType<SQLOperation> = z.object({
  type: z.literal("operation"),
  operator: SQLOperatorSchema,
  operands: z.array(SQLComputedColumnAstSchema),
});

const SQLBracketsSchema: z.ZodType<SQLBrackets> = z.object({
  type: z.literal("brackets"),
  expression: SQLComputedColumnAstSchema,
});

const computedColumnSchema = z.object({
  name: z.string(),
  ast: SQLComputedColumnAstSchema,
});

export type ComputedColumn = z.infer<typeof computedColumnSchema>;

const JsonColumnSchemaSchema = z.array(
  z.object({
    tableName: z.string(),
    jsonColumnName: z.string(),
    jsonSchema: z.array(
      z.object({
        name: z.string(),
        relation: z.null(),
        type: z.string(),
      })
    ),
  })
);

const baseSchema = {
  table: z.string(),
  computedColumns: z.array(computedColumnSchema).optional(),
  whereAndArray: QueryWhereAndArraySchema,
  jsonColumnSchema: JsonColumnSchemaSchema.optional(),
};

const findManySchema = z
  .object({
    ...baseSchema,
    operation: z.literal("findMany"),
    operationParameters: z
      .object({
        columns: z.record(z.string(), z.array(z.string())),
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

const countSchema = z
  .object({
    ...baseSchema,
    operation: z.literal("count"),
    operationParameters: z
      .object({
        columns: z.array(z.string()),
      })
      .strict(),
  })
  .strict();

const countWithJoinSchema = z
  .object({
    ...baseSchema,
    operation: z.literal("countWithJoin"),

    operationParameters: z
      .object({
        joins: z.array(
          z.object({
            table: z.string(),
            joinPath: z.string(),
            joinType: z.enum(["inner", "left", "right"]),
          })
        ),
        count: z.array(z.string()),
      })
      .strict(),
  })
  .strict();

const countRelationsSchema = z
  .object({
    ...baseSchema,
    operation: z.literal("countRelations"),
    operationParameters: z
      .object({
        columns: z.array(z.string()),
        relationsToCount: z.array(
          z.object({
            name: z.string(),
            distinct: z.string().nullable(),
          })
        ),
        orderBy: z
          .object({
            relation: z.string(),
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
        min: z.array(z.string()).nullable(),
        max: z.array(z.string()).nullable(),
        avg: z.array(z.string()).nullable(),
        sum: z.array(z.string()).nullable(),
        count: z.array(z.string()).nullable(),
        median: z.array(z.string()).nullable(),
      })
      .strict(),
  })
  .strict();

const averageDurationBetweenTwoDatesSchema = z
  .object({
    ...baseSchema,
    operation: z.literal("averageDurationBetweenTwoDates"),
    operationParameters: z
      .object({
        columnA: z.string(),
        columnB: z.string(),
      })
      .strict(),
  })
  .strict();

const aggregateByDateIntervalSchema = z
  .object({
    ...baseSchema,
    operation: z.literal("aggregateByDateInterval"),
    operationParameters: z
      .object({
        interval: z.enum(["day", "week", "month", "year"]),
        aggregationType: z.enum(["min", "max", "count", "sum", "avg"]),
        dateColumn: z.string(),
        aggregateColumn: z.string(),
      })
      .strict(),
  })
  .strict();

const countByTemporalComponentSchema = z
  .object({
    ...baseSchema,
    operation: z.literal("countByTemporalComponent"),
    operationParameters: z
      .object({
        component: z.enum(["Day", "Month"]),
        dateColumn: z.string(),
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
        joins: z.array(
          z.object({
            table: z.string(),
            joinPath: z.string(),
            joinType: z.enum(["inner", "left", "right"]),
          })
        ),
        groupBy: z.array(z.string()),
        count: z.object({ columns: z.array(z.string()) }).nullable(),
        sum: z.object({ columns: z.array(z.string()) }).nullable(),
        min: z.object({ columns: z.array(z.string()) }).nullable(),
        max: z.object({ columns: z.array(z.string()) }).nullable(),
        avg: z.object({ columns: z.array(z.string()) }).nullable(),
        orderBy: z.object({
          function: z.enum(["count", "sum", "min", "max", "avg"]),
          column: z.string(),
          direction: z.enum(["asc", "desc"]),
        }),
        limit: z.number(),
      })
      .strict(),
  })
  .strict();

export const QuerySchema = z.discriminatedUnion("operation", [
  findManySchema,
  findDistinctSchema,
  countSchema,
  countWithJoinSchema,
  countRelationsSchema,
  aggregateSchema,
  groupBySchema,
  aggregateByDateIntervalSchema,
  countByTemporalComponentSchema,
  averageDurationBetweenTwoDatesSchema,
]);

export type Query = z.infer<typeof QuerySchema>;
