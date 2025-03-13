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

const ConstantNodeSchema = z.object({
  mathjs: z.literal("ConstantNode"),
  value: z.union([z.number(), z.string(), z.boolean()]),
  valueType: z.string().optional(),
});

const SymbolNodeSchema = z.object({
  mathjs: z.literal("SymbolNode"),
  name: z.string(),
});

const OperatorNodeSchema = z.object({
  mathjs: z.literal("OperatorNode"),
  op: z.string(),
  fn: z.string(),
  args: z.array(z.lazy(() => MathsjsAstSchema)),
});

const ParenthesisNodeSchema = z.object({
  mathjs: z.literal("ParenthesisNode"),
  content: z.lazy(() => MathsjsAstSchema),
});

const MathsjsAstSchema: z.ZodTypeAny = z.discriminatedUnion("mathjs", [
  ConstantNodeSchema,
  SymbolNodeSchema,
  OperatorNodeSchema,
  ParenthesisNodeSchema,
]);

const computedColumnSchema = z.object({
  name: z.string(),
  ast: MathsjsAstSchema,
  type: z.string(),
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
        groupBy: z.array(
          z.object({
            column: z.string(),
            join: z.record(z.string(), z.string()).nullable(),
          })
        ),
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

const findManyBelowPercentageOfMedianSchema = z
  .object({
    ...baseSchema,
    operation: z.literal("findManyBelowPercentageOfMedian"),
    operationParameters: z
      .object({
        selectColumns: z.array(z.string()),
        columnToCalculateMedian: z.string(),
        belowPercentage: z.number(),
      })
      .strict(),
  })
  .strict();

export const QuerySchema = z.discriminatedUnion("operation", [
  findManySchema,
  findDistinctSchema,
  countSchema,
  countRelationsSchema,
  aggregateSchema,
  groupBySchema,
  aggregateByDateIntervalSchema,
  countByTemporalComponentSchema,
  averageDurationBetweenTwoDatesSchema,
  findManyBelowPercentageOfMedianSchema,
]);

export type Query = z.infer<typeof QuerySchema>;
