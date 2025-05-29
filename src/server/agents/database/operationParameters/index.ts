import assert from "assert";
import { z } from "zod";
import { AzureChatOpenAI } from "@langchain/openai";
import { Annotation, START, StateGraph } from "@langchain/langgraph";
import { stringArrayToZodEnum } from "../../utils/zodHelpers";
import type { Schema } from "~/server/db/schema";
import { getPrompt } from "../../utils/getPrompt";
import { buildTableSchemaStringFromTableSchema } from "../utils/schemaFormatters";
import YAML from "yaml";
import { operationDocs } from "../utils/operationDocs";
import type { Operation } from "../types";
import type {
  GroupByDateIntervalQuery,
  AggregateQuery,
  CountByTemporalComponentQuery,
  CountQuery,
  CountRelationsQuery,
  CountWithJoinQuery,
  FindDistinctQuery,
  FindManyQuery,
  GroupByQuery,
} from "~/server/userDatabaseConnector/types";
import { generateJoinedTables } from "../utils/tableRelations";

interface RequestParams {
  operation: Exclude<Operation, "findDistinctByEditDistance">;
  schema: Schema;
  tableSchema: Schema[number];
  tableName: string;
  question: string;
}

// Helper function to transform an array value to null if it's empty
const transformEmptyArrayToNull = (value: string[] | null): string[] | null => {
  return Array.isArray(value) && value.length === 0 ? null : value;
};

export function operationParametersAgent(params: RequestParams) {
  const OperationParametersState = Annotation.Root({
    next: Annotation<string>({
      reducer: (x, y) => y,
    }),
    columnNames: Annotation<string[]>({
      reducer: (x, y) => y,
    }),
    computedColumnNames: Annotation<string[]>({
      reducer: (x, y) => y,
    }),
    dateColumnNames: Annotation<string[]>({
      reducer: (x, y) => y,
    }),
    numericalColumnNames: Annotation<string[]>({
      reducer: (x, y) => y,
    }),
    relationListNames: Annotation<string[]>({
      reducer: (x, y) => y,
    }),
  });

  const OutputAnnotation = Annotation.Root({
    operationParameters: Annotation<Record<string, unknown>>({
      reducer: (x, y) => ({ ...x, ...y }),
    }),
  });

  const model = new AzureChatOpenAI({
    model: "gpt-4.1",
    deploymentName: "gpt-4.1",
    temperature: 0,
  });

  const prepareForOperation = async (
    _state: typeof OperationParametersState.State
  ) => {
    const {
      columns = [],
      outwardRelations: relations = [],
      computedColumns = [],
    } = params.tableSchema;
    assert(columns.length > 0, "Table has no columns");

    const columnNames = columns.map(({ name }) => name);
    const computedColumnNames = computedColumns.map(({ name }) => name);
    const dateColumnNames = columns
      .filter(({ type }) => type === "DateTime")
      .map(({ name }) => name);
    const numericalColumnNames = [
      ...columns
        .filter(({ type }) => ["number"].includes(type))
        .map(({ name }) => name),
      ...computedColumns.map(({ name }) => name),
    ];
    // - To Many relations
    const relationListNames = relations
      .filter(({ isList }) => isList)
      .map(({ name }) => name);

    return {
      next: params.operation,
      columnNames,
      computedColumnNames,
      dateColumnNames,
      numericalColumnNames,
      relationListNames,
    };
  };

  const determineParamsForSchema = async <T extends z.ZodTypeAny>({
    schema,
    tableSchema,
    queryCurrentState,
  }: {
    schema: T;
    tableSchema?: string;
    queryCurrentState?: string;
  }): Promise<z.infer<T>> => {
    const queryExtenderPrompt = await getPrompt("extend_query");
    const queryExtender = model.withStructuredOutput(schema, {
      method: "jsonSchema",
      strict: true,
    });
    const response: unknown = await queryExtenderPrompt
      .pipe(queryExtender)
      .invoke({
        table: params.tableName,
        tableSchema:
          tableSchema ??
          buildTableSchemaStringFromTableSchema(params.tableSchema),
        operation: params.operation,
        operationDocs: YAML.stringify(operationDocs[params.operation]),
        question: params.question,
        queryCurrentState,
      });

    return response;
  };

  const findMany = async (state: typeof OperationParametersState.State) => {
    const generateSelect = async () => {
      const { iqlPaths, uniqueTableNames } = generateJoinedTables(
        params.schema,
        params.tableName,
        3
      );

      const tableSelectSchema = z.object({
        tables: z.array(stringArrayToZodEnum(Object.keys(iqlPaths))),
      });
      const tableSchemas = uniqueTableNames.map((tableName) =>
        buildTableSchemaStringFromTableSchema(
          params.schema.find((table) => table.name === tableName)!
        )
      );
      const tableSelectionPrompt = await getPrompt("findmany-select-tables");
      const tableSelector = model.withStructuredOutput(tableSelectSchema, {
        method: "jsonSchema",
        strict: true,
      });
      const tableSelectResult = await tableSelectionPrompt
        .pipe(tableSelector)
        .invoke({
          table: params.tableName,
          tableSchemas: YAML.stringify(tableSchemas),
          operation: params.operation,
          operationDocs: YAML.stringify(operationDocs[params.operation]),
          question: params.question,
        });
      const selectableTableColumns: Record<string, string[]> = {};
      const tablesInQuery: string[] = [];
      tableSelectResult.tables.forEach((tablePath) => {
        const tableName = iqlPaths[tablePath];
        assert(tableName, `Table ${tablePath} not found in schema`);
        const tableSchema = params.schema.find((t) => t.name === tableName);
        assert(tableSchema, `Table ${tableName} not found in schema`);
        tablesInQuery.push(tableName);
        selectableTableColumns[tablePath] = [
          ...tableSchema.columns.map((col) => col.name),
          ...tableSchema.computedColumns.map((col) => col.name),
        ];
      });
      const selectColumnsSchema = z.object({
        columns: z.object(
          Object.keys(selectableTableColumns).reduce(
            (
              acc: Record<
                string,
                z.ZodNullable<z.ZodArray<z.ZodEnum<[string, ...string[]]>>>
              >,
              key
            ) => {
              if (!selectableTableColumns[key]) {
                return acc;
              }
              acc[key] = z
                .array(stringArrayToZodEnum(selectableTableColumns[key]))
                .nullable();
              return acc;
            },
            {} as Record<
              string,
              z.ZodNullable<z.ZodArray<z.ZodEnum<[string, ...string[]]>>>
            >
          )
        ),
      });
      const tableSchemasForSchemasInQuery = tablesInQuery.map((tableName) =>
        buildTableSchemaStringFromTableSchema(
          params.schema.find((table) => table.name === tableName)!
        )
      );
      const selectResult = await determineParamsForSchema({
        schema: selectColumnsSchema,
        tableSchema: YAML.stringify(tableSchemasForSchemasInQuery),
      });
      const selectResultFiltered = Object.entries(selectResult.columns).reduce(
        (acc: Record<string, string[]>, [key, value]) => {
          if (value?.length && value.length > 0) {
            acc[key] = value;
          }
          return acc;
        },
        {}
      );
      return { columns: selectResultFiltered };
    };

    const orderBySchema = z.object({
      orderBy: z
        .object({
          direction: z.enum(["asc", "desc"]),
          column: stringArrayToZodEnum([
            ...state.columnNames,
            ...state.computedColumnNames,
          ]).describe("The column to order by"),
        })
        .nullable(),
      limit: z.number().describe("The number of records to return"),
    });
    const [selectResult, orderByResult] = await Promise.all([
      generateSelect(),
      determineParamsForSchema({
        schema: orderBySchema,
      }),
    ]);
    return {
      operationParameters: {
        ...selectResult,
        ...orderByResult,
      } satisfies FindManyQuery["operationParameters"],
    };
  };

  const groupBy = async (_state: typeof OperationParametersState.State) => {
    const prepareJoins = async () => {
      // Only allow joins 1 table over from the starting table
      const tableCandidates = generateJoinedTables(
        params.schema,
        params.tableName,
        1
      );

      if (
        tableCandidates.uniqueTableNames.filter(
          (name) => name !== params.tableName
        ).length === 0
      ) {
        return {
          joins: null,
        };
      }

      const tableSelectionPrompt = await getPrompt("select_join_tables");

      const joinTableZodSchemas = Object.entries(tableCandidates.iqlPaths)
        .filter(([_path, tableName]) => tableName !== params.tableName)
        .map(([path, tableName]) => {
          return z.object({
            table: z.literal(tableName),
            joinPath: z.literal(path),
            joinType: z.enum(["inner", "left", "right"]),
          });
        });

      const joins = model.withStructuredOutput(
        z.object({
          joins: z
            .array(
              z.union(
                joinTableZodSchemas as unknown as [
                  z.ZodTypeAny,
                  z.ZodTypeAny,
                  ...z.ZodTypeAny[]
                ]
              )
            )
            .nullable()
            .describe(
              `The tables to join to the ${params.tableName} table, if any`
            ),
        }),
        {
          method: "jsonSchema",
          strict: true,
        }
      );

      const tableSchemas = tableCandidates.uniqueTableNames.map((tableName) =>
        buildTableSchemaStringFromTableSchema(
          params.schema.find((table) => table.name === tableName)!
        )
      );

      const tableJoinResult = (await tableSelectionPrompt.pipe(joins).invoke({
        table: params.tableName,
        tableSchemas: YAML.stringify(tableSchemas),
        operationName: params.operation,
        operation: params.operation,
        operationDocs: YAML.stringify(operationDocs[params.operation]),
        question: params.question,
      })) as {
        joins:
          | {
              table: string;
              joinPath: string;
              joinType: "inner" | "left" | "right";
            }[]
          | null;
      };

      return tableJoinResult;
    };

    const tableJoinResult = await prepareJoins();

    const tablesInQuery = tableJoinResult.joins
      ? tableJoinResult.joins.map((join) => join.table).concat(params.tableName)
      : [params.tableName];

    const tableSchemasForSchemasInQuery = tablesInQuery.map((tableName) =>
      buildTableSchemaStringFromTableSchema(
        params.schema.find((table) => table.name === tableName)!
      )
    );

    const gbColumnNames = tablesInQuery.reduce((acc: string[], tableName) => {
      const tableSchema = params.schema.find(
        (table) => table.name === tableName
      );
      assert(tableSchema, `Table ${tableName} not found in schema`);
      const columns = tableSchema.columns.map(
        (col) => `${tableName}.${col.name}`
      );
      acc.push(...columns);
      return acc;
    }, []);

    const gbNumericalColumnNames = tablesInQuery.reduce(
      (acc: string[], tableName) => {
        const tableSchema = params.schema.find(
          (table) => table.name === tableName
        );
        assert(tableSchema, `Table ${tableName} not found in schema`);
        const computedColumns = tableSchema.computedColumns.map(
          (col) => `${tableName}.${col.name}`
        );
        const columns = tableSchema.columns
          .filter(({ type }) => ["number"].includes(type))
          .map((col) => `${tableName}.${col.name}`)
          .concat(computedColumns);
        acc.push(...columns);
        return acc;
      },
      [] as string[]
    );

    const defineGroupByColumns = async () => {
      const groupByColumns = z.object({
        groupBy: z
          .array(stringArrayToZodEnum(gbColumnNames))
          .describe("The column or columns to group the results by"),
      });
      const groupColumnParams = await determineParamsForSchema({
        schema: groupByColumns,
        tableSchema: YAML.stringify(tableSchemasForSchemasInQuery),
        queryCurrentState: YAML.stringify(tableJoinResult),
      });

      return { groupBy: groupColumnParams.groupBy };
    };

    const aggregateSchema = z.object({
      count: z
        .object({
          columns: z
            .array(stringArrayToZodEnum(gbColumnNames))
            .describe("The columns to count"),
        })
        .nullable(),
      sum:
        gbNumericalColumnNames?.length > 0
          ? z
              .object({
                columns: z
                  .array(stringArrayToZodEnum(gbNumericalColumnNames))
                  .describe("The columns to sum"),
              })
              .nullable()
          : z.null(),
      min:
        gbNumericalColumnNames?.length > 0
          ? z
              .object({
                columns: z
                  .array(stringArrayToZodEnum(gbNumericalColumnNames))
                  .describe("The columns to find the minimum of"),
              })
              .nullable()
          : z.null(),
      max:
        gbNumericalColumnNames?.length > 0
          ? z
              .object({
                columns: z
                  .array(stringArrayToZodEnum(gbNumericalColumnNames))
                  .describe("The columns to find the maximum of"),
              })
              .nullable()
          : z.null(),
      avg:
        gbNumericalColumnNames?.length > 0
          ? z
              .object({
                columns: z
                  .array(stringArrayToZodEnum(gbNumericalColumnNames))
                  .describe("The columns to find the average of"),
              })
              .nullable()
          : z.null(),
    });

    const orderByFunctionSchema = z.object({
      orderByFunction:
        gbNumericalColumnNames?.length > 0
          ? z.enum(["count", "sum", "min", "max", "avg"] as const)
          : z.enum(["count"] as const),
    });

    const [groupColumnParams, aggregateOperationParams, orderByFunctionObj] =
      await Promise.all([
        defineGroupByColumns(),
        determineParamsForSchema({
          schema: aggregateSchema,
          tableSchema: YAML.stringify(tableSchemasForSchemasInQuery),
          queryCurrentState: YAML.stringify(tableJoinResult),
        }),
        determineParamsForSchema({
          schema: orderByFunctionSchema,
          tableSchema: YAML.stringify(tableSchemasForSchemasInQuery),
          queryCurrentState: YAML.stringify(tableJoinResult),
        }),
      ]);

    const orderBySchema = z.object({
      orderBy: z.object({
        function: z.literal(orderByFunctionObj.orderByFunction),
        column:
          orderByFunctionObj.orderByFunction === "count"
            ? stringArrayToZodEnum(gbColumnNames)
            : stringArrayToZodEnum(gbNumericalColumnNames),
        direction: z.enum(["asc", "desc"]),
      }),
      limit: z.number().describe("The number of records to return"),
    });

    const orderByOperationParams = await determineParamsForSchema({
      schema: orderBySchema,
    });

    return {
      operationParameters: {
        ...tableJoinResult,
        ...groupColumnParams,
        ...aggregateOperationParams,
        ...orderByOperationParams,
      } satisfies GroupByQuery["operationParameters"],
    };
  };

  const countWithJoin = async (
    _state: typeof OperationParametersState.State
  ) => {
    const prepareJoins = async () => {
      // Only allow joins 1 table over from the starting table
      const tableCandidates = generateJoinedTables(
        params.schema,
        params.tableName,
        1
      );

      if (
        tableCandidates.uniqueTableNames.filter(
          (name) => name !== params.tableName
        ).length === 0
      ) {
        throw new Error("No tables to join");
      }

      const tableSelectionPrompt = await getPrompt("select_join_tables");

      const joinTableZodSchemas = Object.entries(tableCandidates.iqlPaths)
        .filter(([_path, tableName]) => tableName !== params.tableName)
        .map(([path, tableName]) => {
          return z.object({
            table: z.literal(tableName),
            joinPath: z.literal(path),
            joinType: z.enum(["inner", "left", "right"]),
          });
        });

      const joins = model.withStructuredOutput(
        z.object({
          joins: z
            .array(
              z.union(
                joinTableZodSchemas as unknown as [
                  z.ZodTypeAny,
                  z.ZodTypeAny,
                  ...z.ZodTypeAny[]
                ]
              )
            )
            .nullable()
            .describe(
              `The tables to join to the ${params.tableName} table, if any`
            ),
        }),
        {
          method: "jsonSchema",
          strict: true,
        }
      );

      const tableSchemas = tableCandidates.uniqueTableNames.map((tableName) =>
        buildTableSchemaStringFromTableSchema(
          params.schema.find((table) => table.name === tableName)!
        )
      );

      const tableJoinResult = (await tableSelectionPrompt.pipe(joins).invoke({
        table: params.tableName,
        tableSchemas: YAML.stringify(tableSchemas),
        operationName: params.operation,
        operation: params.operation,
        operationDocs: YAML.stringify(operationDocs[params.operation]),
        question: params.question,
      })) as {
        joins: {
          table: string;
          joinPath: string;
          joinType: "inner" | "right" | "left";
        }[];
      };

      return tableJoinResult;
    };

    const tableJoinResult = await prepareJoins();

    const tablesInQuery = tableJoinResult.joins
      ? tableJoinResult.joins.map((join) => join.table).concat(params.tableName)
      : [params.tableName];

    const columnNames = tablesInQuery.reduce((acc: string[], tableName) => {
      const tableSchema = params.schema.find(
        (table) => table.name === tableName
      );
      assert(tableSchema, `Table ${tableName} not found in schema`);
      const columns = tableSchema.columns.map(
        (col) => `${tableName}.${col.name}`
      );
      acc.push(...columns);
      return acc;
    }, []);

    const countColumnsSchema = z.object({
      count: z
        .array(stringArrayToZodEnum(columnNames))
        .describe("The columns to count"),
    });

    const countColumnsParams = await determineParamsForSchema({
      schema: countColumnsSchema,
    });

    return {
      operationParameters: {
        ...tableJoinResult,
        ...countColumnsParams,
      } satisfies CountWithJoinQuery["operationParameters"],
    };
  };

  const countRelations = async (
    state: typeof OperationParametersState.State
  ) => {
    const schema = z.object({
      columns: z.array(stringArrayToZodEnum(state.columnNames)),
      relations: z
        .array(stringArrayToZodEnum(state.relationListNames))
        .describe("The relations to count"),
      orderBy: z
        .object({
          direction: z.enum(["asc", "desc"]),
          relation: stringArrayToZodEnum(state.relationListNames).describe(
            "The relation to order by (must be in the relations array)"
          ),
        })
        .nullable(),
      limit: z.number().describe("The number of records to return"),
    });

    const operationParameters = await determineParamsForSchema({ schema });

    const relationsToCount = await Promise.all(
      operationParameters.relations.map(async (relationName) => {
        const relationTargetTableName =
          params.tableSchema.outwardRelations.find(
            (relation) => relation.name === relationName
          )?.targetTable.name;
        assert(
          relationTargetTableName,
          `Target Table not found for ${relationName}`
        );
        const relationTableSchema = params.schema.find(
          (table) => table.name === relationTargetTableName
        );
        assert(relationTableSchema, "Relation Table not found");
        return await determineParamsForSchema({
          schema: z.object({
            name: z.literal(relationName),
            distinct: stringArrayToZodEnum(
              relationTableSchema.columns.map((column) => column.name)
            )
              .nullable()
              .describe(
                "A column in the related table that the count should be distinct by (set null if not necessary). Count is automatically distinct to the starting starting table"
              ),
          }),
        });
      })
    );

    const { relations, ...restParameters } = operationParameters;

    return {
      operationParameters: {
        ...restParameters,
        relationsToCount,
      } satisfies CountRelationsQuery["operationParameters"],
    };
  };

  const aggregate = async (state: typeof OperationParametersState.State) => {
    const schema = z.object({
      avg: z.array(stringArrayToZodEnum(state.numericalColumnNames)).nullable(),
      sum: z.array(stringArrayToZodEnum(state.numericalColumnNames)).nullable(),
      min: z.array(stringArrayToZodEnum(state.numericalColumnNames)).nullable(),
      max: z.array(stringArrayToZodEnum(state.numericalColumnNames)).nullable(),
      count: z.array(stringArrayToZodEnum(state.columnNames)).nullable(),
      median: z
        .array(stringArrayToZodEnum(state.numericalColumnNames))
        .nullable(),
    });
    const rawOperationParameters = await determineParamsForSchema({ schema });

    const operationParameters: AggregateQuery["operationParameters"] = {
      avg: transformEmptyArrayToNull(rawOperationParameters.avg),
      sum: transformEmptyArrayToNull(rawOperationParameters.sum),
      min: transformEmptyArrayToNull(rawOperationParameters.min),
      max: transformEmptyArrayToNull(rawOperationParameters.max),
      count: transformEmptyArrayToNull(rawOperationParameters.count),
      median: transformEmptyArrayToNull(rawOperationParameters.median),
    };

    return {
      operationParameters:
        operationParameters satisfies AggregateQuery["operationParameters"],
    };
  };

  const count = async (state: typeof OperationParametersState.State) => {
    const schema = z.object({
      columns: z
        .array(stringArrayToZodEnum(state.columnNames.concat(["_all"])))
        .describe(
          "_all is a count of all rows, A column is a count of non-null values in the column"
        ),
    });
    const operationParameters = await determineParamsForSchema({ schema });
    return {
      operationParameters:
        operationParameters satisfies CountQuery["operationParameters"],
    };
  };

  const groupByDateInterval = async (
    state: typeof OperationParametersState.State
  ) => {
    const aggregateSchema = z.object({
      dateColumn: stringArrayToZodEnum(state.dateColumnNames),
      interval: z.enum(["day", "week", "month", "year"]),
      avg: z
        .array(stringArrayToZodEnum(state.numericalColumnNames))
        .nullable()
        .describe("AVG of the column values over the interval"),
      sum: z
        .array(stringArrayToZodEnum(state.numericalColumnNames))
        .nullable()
        .describe("SUM of the column values over the interval"),
      min: z
        .array(stringArrayToZodEnum(state.numericalColumnNames))
        .nullable()
        .describe("MIN value for column over the interval"),
      max: z
        .array(stringArrayToZodEnum(state.numericalColumnNames))
        .nullable()
        .describe("MAX value for column over the interval"),
      count: z
        .array(stringArrayToZodEnum(state.columnNames))
        .nullable()
        .describe("COUNT of the column over the interval"),
    });

    const orderByFunctionSchema = z.object({
      orderByFunction:
        state.numericalColumnNames?.length > 0
          ? z.enum(["count", "sum", "min", "max", "avg"] as const)
          : z.enum(["count"] as const),
    });

    const [aggregateParams, orderByFunctionObj] = await Promise.all([
      determineParamsForSchema({
        schema: aggregateSchema,
      }),
      determineParamsForSchema({
        schema: orderByFunctionSchema,
      }),
    ]);

    const orderBySchema = z.object({
      orderBy: z.object({
        function: z.literal(orderByFunctionObj.orderByFunction),
        column:
          orderByFunctionObj.orderByFunction === "count"
            ? stringArrayToZodEnum(state.columnNames)
            : stringArrayToZodEnum(state.numericalColumnNames),
        direction: z.enum(["asc", "desc"]),
      }),
      limit: z.number().describe("The number of records to return"),
    });

    const orderByOperationParams = await determineParamsForSchema({
      schema: orderBySchema,
      queryCurrentState: YAML.stringify(aggregateParams),
    });

    const operationParameters = {
      ...aggregateParams,
      avg: transformEmptyArrayToNull(aggregateParams.avg),
      sum: transformEmptyArrayToNull(aggregateParams.sum),
      min: transformEmptyArrayToNull(aggregateParams.min),
      max: transformEmptyArrayToNull(aggregateParams.max),
      count: transformEmptyArrayToNull(aggregateParams.count),
      ...orderByOperationParams,
    };

    return {
      operationParameters:
        operationParameters satisfies GroupByDateIntervalQuery["operationParameters"],
    };
  };

  const countByTemporalComponent = async (
    state: typeof OperationParametersState.State
  ) => {
    const schema = z.object({
      component: z.enum(["Day", "Month"]),
      dateColumn: stringArrayToZodEnum(state.dateColumnNames),
    });
    const operationParameters = await determineParamsForSchema({ schema });
    return {
      operationParameters:
        operationParameters satisfies CountByTemporalComponentQuery["operationParameters"],
    };
  };

  const findDistinct = async (state: typeof OperationParametersState.State) => {
    const schema = z.object({
      column: stringArrayToZodEnum(state.columnNames).describe(
        "The column to select"
      ),
    });
    const operationParameters = await determineParamsForSchema({ schema });
    return {
      operationParameters:
        operationParameters satisfies FindDistinctQuery["operationParameters"],
    };
  };

  const workflow = new StateGraph({
    input: OperationParametersState,
    output: OutputAnnotation,
  })
    .addNode("prepare_for_operation", prepareForOperation)
    .addNode("findMany", findMany)
    .addNode("findDistinct", findDistinct)
    .addNode("count", count)
    .addNode("countWithJoin", countWithJoin)
    .addNode("countRelations", countRelations)
    .addNode("aggregate", aggregate)
    .addNode("groupByDateInterval", groupByDateInterval)
    .addNode("countByTemporalComponent", countByTemporalComponent)
    .addNode("groupBy", groupBy);

  workflow
    .addEdge(START, "prepare_for_operation")
    .addConditionalEdges(
      "prepare_for_operation",
      (x: typeof OperationParametersState.State) => x.next
    );

  return workflow.compile();
}
