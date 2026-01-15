import assert from "assert";
import { Annotation, START, StateGraph } from "@langchain/langgraph";
import type { Schema } from "@repo/types";
import type { Operation } from "../types";
import type {
  AggregateQuery,
  AggregateGroupsQuery,
  CountQuery,
  CountRelationsQuery,
  FindDistinctQuery,
  FindManyQuery,
  GroupByQuery,
} from "@repo/types";
import { defineFindManyOperationParameters } from "./findMany";
import { defineGroupByOperationParameters } from "./groupBy";
import { defineCountRelationsOperationParameters } from "./countRelations";
import { defineCountOperationParameters } from "./count";
import { defineAggregateOperationParameters } from "./aggregate";
import { defineFindDistinctOperationParameters } from "./findDistinct";
import { defineAggregateGroupsOperationParameters } from "./aggregateGroups";

interface RequestParams {
  operation: Exclude<Operation, "findDistinctByEditDistance">;
  schema: Schema;
  tableSchema: Schema[number];
  tableName: string;
  question: string;
  userContext: Record<string, string | number>;
  agentId: string | number;
}

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

  const prepareForOperation = async (
    _state: typeof OperationParametersState.State,
  ) => {
    const tableColumns = params.tableSchema.columns;
    const tableRelations = params.tableSchema.outwardRelations ?? [];
    const tableComputedColumns = params.tableSchema.computedColumns ?? [];
    assert(tableColumns.length > 0, "Table has no columns");

    const columnNames = tableColumns.map(
      ({ name }: Schema[number]["columns"][number]) => name,
    );
    const computedColumnNames = tableComputedColumns.map(
      ({ name }: NonNullable<Schema[number]["computedColumns"]>[number]) =>
        name,
    );
    const dateColumnNames = tableColumns
      .filter(
        ({ type, effectiveType }: Schema[number]["columns"][number]) =>
          (effectiveType ?? type) === "DateTime",
      )
      .map(({ name }: Schema[number]["columns"][number]) => name);
    const numericTypes = new Set([
      "number",
      "integer",
      "bigint",
      "decimal",
      "float",
    ]);
    const numericalColumnNames = [
      ...tableColumns
        .filter(({ type, effectiveType }: Schema[number]["columns"][number]) =>
          effectiveType
            ? numericTypes.has(effectiveType)
            : numericTypes.has(type),
        )
        .map(({ name }: Schema[number]["columns"][number]) => name),
      ...tableComputedColumns.map(
        ({ name }: NonNullable<Schema[number]["computedColumns"]>[number]) =>
          name,
      ),
    ];
    // - To Many relations
    const relationListNames = tableRelations
      .filter(
        ({ isList }: Schema[number]["outwardRelations"][number]) => isList,
      )
      .map(({ name }: Schema[number]["outwardRelations"][number]) => name);

    return {
      next: params.operation,
      columnNames,
      computedColumnNames,
      dateColumnNames,
      numericalColumnNames,
      relationListNames,
    };
  };

  const findMany = async (_state: typeof OperationParametersState.State) => {
    const operationParams = await defineFindManyOperationParameters({
      schema: params.schema,
      tableSchema: params.tableSchema,
      tableName: params.tableName,
      question: params.question,
      operation: "findMany",
      userContext: params.userContext,
      agentId: params.agentId,
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
      userContext: params.userContext,
      agentId: params.agentId,
    });
    assert(
      operationParameters,
      "Failed to define groupBy operation parameters",
    );
    return {
      operationParameters: {
        ...operationParameters,
      } satisfies GroupByQuery["operationParameters"],
    };
  };

  const countRelations = async (
    _state: typeof OperationParametersState.State,
  ) => {
    const operationParameters = await defineCountRelationsOperationParameters({
      schema: params.schema,
      tableSchema: params.tableSchema,
      tableName: params.tableName,
      question: params.question,
      operation: "countRelations",
      userContext: params.userContext,
      agentId: params.agentId,
    });
    assert(
      operationParameters,
      "Failed to define countRelations operation parameters",
    );
    return {
      operationParameters: {
        ...operationParameters,
      } satisfies CountRelationsQuery["operationParameters"],
    };
  };

  const aggregate = async (_state: typeof OperationParametersState.State) => {
    const operationParameters = await defineAggregateOperationParameters({
      schema: params.schema,
      tableSchema: params.tableSchema,
      tableName: params.tableName,
      question: params.question,
      operation: "aggregate",
      userContext: params.userContext,
      agentId: params.agentId,
    });
    return {
      operationParameters:
        operationParameters satisfies AggregateQuery["operationParameters"],
    };
  };

  const aggregateGroups = async (
    _state: typeof OperationParametersState.State,
  ) => {
    const operationParameters = await defineAggregateGroupsOperationParameters({
      schema: params.schema,
      tableSchema: params.tableSchema,
      tableName: params.tableName,
      question: params.question,
      operation: "aggregateGroups",
      userContext: params.userContext,
      agentId: params.agentId,
    });
    assert(
      operationParameters,
      "Failed to define aggregateGroups operation parameters",
    );
    return {
      operationParameters:
        operationParameters satisfies AggregateGroupsQuery["operationParameters"],
    };
  };

  const count = async (_state: typeof OperationParametersState.State) => {
    const operationParameters = await defineCountOperationParameters({
      schema: params.schema,
      tableSchema: params.tableSchema,
      tableName: params.tableName,
      question: params.question,
      operation: "count",
      userContext: params.userContext,
      agentId: params.agentId,
    });
    return {
      operationParameters:
        operationParameters satisfies CountQuery["operationParameters"],
    };
  };

  const findDistinct = async (
    _state: typeof OperationParametersState.State,
  ) => {
    const operationParameters = await defineFindDistinctOperationParameters({
      schema: params.schema,
      tableSchema: params.tableSchema,
      tableName: params.tableName,
      question: params.question,
      operation: "findDistinct",
      userContext: params.userContext,
      agentId: params.agentId,
    });
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
    .addNode("countRelations", countRelations)
    .addNode("aggregate", aggregate)
    .addNode("aggregateGroups", aggregateGroups)
    .addNode("groupBy", groupBy);

  workflow
    .addEdge(START, "prepare_for_operation")
    .addConditionalEdges(
      "prepare_for_operation",
      (x: typeof OperationParametersState.State) => x.next,
    );

  return workflow.compile();
}
