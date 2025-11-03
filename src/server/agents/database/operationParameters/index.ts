import assert from "assert";
import { z } from "zod";
import { getAIModel } from "~/server/agents/utils/getAIModel";
import { Annotation, START, StateGraph } from "@langchain/langgraph";
import { stringArrayToZodEnum } from "../../utils/zodHelpers";
import type { Schema } from "~/server/db/schema";
import { getPrompt } from "../../utils/getPrompt";
import { buildTableSchemaStringFromTableSchema } from "../utils/schemaFormatters";
import YAML from "yaml";
import { operationDocs } from "../utils/operationDocs";
import type { Operation } from "../types";
import type {
  AggregateQuery,
  CountQuery,
  CountRelationsQuery,
  CountWithJoinQuery,
  FindDistinctQuery,
  FindManyQuery,
  GroupByQuery,
} from "~/server/userDatabaseConnector/types";
import { defineFindManyOperationParameters } from "./findMany";
import { defineGroupByOperationParameters } from "./groupBy";
import { defineCountWithJoinOperationParameters } from "./countWithJoin";
import { defineCountRelationsOperationParameters } from "./countRelations";

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

  const model = getAIModel("azure:gpt-4.1");

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
    const queryExtenderPrompt = await getPrompt("extend_query:f100254a");
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

  const findMany = async (_state: typeof OperationParametersState.State) => {
    const operationParams = await defineFindManyOperationParameters({
      schema: params.schema,
      tableSchema: params.tableSchema,
      tableName: params.tableName,
      question: params.question,
      operation: "findMany",
    });
    assert(operationParams, "Failed to define findMany operation parameters");
    return {
      operationParameters: {
        ...operationParams,
      } satisfies FindManyQuery["operationParameters"],
    };
  };

  const groupBy = async (_state: typeof OperationParametersState.State) => {
    const operationParameters = await defineGroupByOperationParameters({
      schema: params.schema,
      tableSchema: params.tableSchema,
      tableName: params.tableName,
      question: params.question,
      operation: "groupBy",
    });
    assert(
      operationParameters,
      "Failed to define groupBy operation parameters"
    );
    return {
      operationParameters: {
        ...operationParameters,
      } satisfies GroupByQuery["operationParameters"],
    };
  };

  const countWithJoin = async (
    _state: typeof OperationParametersState.State
  ) => {
    const operationParameters = await defineCountWithJoinOperationParameters({
      schema: params.schema,
      tableSchema: params.tableSchema,
      tableName: params.tableName,
      question: params.question,
      operation: "countWithJoin",
    });
    assert(
      operationParameters,
      "Failed to define countWithJoin operation parameters"
    );
    return {
      operationParameters: {
        ...operationParameters,
      } satisfies CountWithJoinQuery["operationParameters"],
    };
  };

  const countRelations = async (
    _state: typeof OperationParametersState.State
  ) => {
    const operationParameters = await defineCountRelationsOperationParameters({
      schema: params.schema,
      tableSchema: params.tableSchema,
      tableName: params.tableName,
      question: params.question,
      operation: "countRelations",
    });
    assert(
      operationParameters,
      "Failed to define countRelations operation parameters"
    );
    return {
      operationParameters: {
        ...operationParameters,
      } satisfies CountRelationsQuery["operationParameters"],
    };
  };

  const aggregate = async (state: typeof OperationParametersState.State) => {
    const schema = z.object({
      avg: z.array(stringArrayToZodEnum(state.numericalColumnNames)).nullable(),
      sum: z.array(stringArrayToZodEnum(state.numericalColumnNames)).nullable(),
      min: z
        .array(
          stringArrayToZodEnum([
            ...state.numericalColumnNames,
            ...state.dateColumnNames,
          ])
        )
        .nullable(),
      max: z
        .array(
          stringArrayToZodEnum([
            ...state.numericalColumnNames,
            ...state.dateColumnNames,
          ])
        )
        .nullable(),
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
      count: z
        .array(stringArrayToZodEnum(state.columnNames.concat(["_all"])))
        .describe(
          "_all is a count of all rows, A column is a count of non-null values in the column"
        ),
      countDistinct: z
        .array(stringArrayToZodEnum(state.columnNames))
        .nullable()
        .describe("The columns to count distinct values for"),
    });
    const operationParameters = await determineParamsForSchema({ schema });
    return {
      operationParameters:
        operationParameters satisfies CountQuery["operationParameters"],
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
    .addNode("groupBy", groupBy);

  workflow
    .addEdge(START, "prepare_for_operation")
    .addConditionalEdges(
      "prepare_for_operation",
      (x: typeof OperationParametersState.State) => x.next
    );

  return workflow.compile();
}
