import { z } from "zod";
import { UserDatabaseConnector } from "~/server/userDatabaseConnector";
import type {
  JsonColumnSchema,
  Query,
  QueryResponse,
} from "~/server/userDatabaseConnector/types";
import { getAIModel } from "~/server/agents/utils/getAIModel";
import assert from "assert";
import { Annotation, END, START, StateGraph } from "@langchain/langgraph";
import {
  tableConditionsSchema,
  dateConditionSchema,
  querySchema,
  questionConditionsSchema,
  whereAndArraySchema,
} from "~/server/userDatabaseConnector/types";
import { generatePrismaWhereArray } from "./utils/generatePrismaWhereArray";
import { operationDocs } from "./utils/operationDocs";
import { getPrompt } from "../utils/getPrompt";
import YAML from "yaml";
import { questionWhereConditionAgent } from "./questionWhere";
import type { DBQuery, Operation } from "./types";
import type {
  TableConditions,
  DateCondition,
  QuestionConditions,
} from "~/server/userDatabaseConnector/types";
import { stringArrayToZodEnum } from "~/server/agents/utils/zodHelpers";
import {
  buildFullSchemaString,
  buildTableSchemaStringFromTableSchema,
} from "./utils/schemaFormatters";
import { operationParametersAgent } from "./operationParameters";
import type { Schema } from "~/server/db/schema";
import { mapJsonToSchema } from "./utils/jsonColumnSchemaMapper";
import { buildConditionsForTable } from "./utils/buildConditionsForTable";
import { whereConditionDocsSummary } from "./utils/whereDocs";
import {
  buildColumnLookup,
  normalizeDateCondition,
  normalizeQueryColumnReferences,
  normalizeQuestionConditions,
  normalizeTableConditions,
} from "./utils/queryNormalization";
import type { ColumnAliasMap } from "./utils/queryNormalization";

interface RequestParams {
  userQuestion: string;
  schema: Schema;
  requestContext: Record<string, string | number>;
  connectorUrl: string;
  connectorSigningKey: string;
}

export const formatAllConditions = (
  query: DBQuery,
  tableConditions: TableConditions,
  questionConditions: QuestionConditions,
  dateCondition: DateCondition
) => {
  const paredTableConditions = tableConditionsSchema.parse(tableConditions);
  const parsedQuestionConditions =
    questionConditionsSchema.parse(questionConditions);
  const parsedDateCondition = dateConditionSchema.parse(dateCondition);

  const whereAndArray = generatePrismaWhereArray(
    paredTableConditions,
    parsedQuestionConditions,
    parsedDateCondition
  );

  query.whereAndArray = whereAndArraySchema.parse(whereAndArray);
  return query;
};

export async function databaseRetrieverAgent(params: RequestParams) {
  const OverallStateAnnotation = Annotation.Root({
    question: Annotation<string>({
      reducer: (x, y) => y,
      default: () => params.userQuestion,
    }),
    schema: Annotation<Schema>({
      reducer: (x, y) => y,
      default: () => params.schema,
    }),
    jsonColumnSchema: Annotation<JsonColumnSchema | null>({
      reducer: (x, y) => y,
      default: () => null,
    }),
    tableSchema: Annotation<Schema[number]>({
      reducer: (x, y) => y,
    }),
    queryResponse: Annotation<QueryResponse | null>({
      reducer: (x, y) => y,
      default: () => null,
    }),
    databaseResponse: Annotation<{
      query: string;
      response: unknown;
    }>({
      reducer: (x, y) => y,
    }),
    columnAliasMap: Annotation<ColumnAliasMap>({
      reducer: (x, y) => y,
      default: () => ({} as ColumnAliasMap),
    }),
    error: Annotation<Record<string, unknown>>({
      reducer: (x, y) => y,
    }),
  });

  const QueryBuilderState = Annotation.Root({
    tableName: Annotation<string>({
      reducer: (x, y) => y,
    }),
    operation: Annotation<Exclude<Operation, "findDistinctByEditDistance">>({
      reducer: (x, y) => y,
    }),
    operationParams: Annotation<Record<string, unknown>>({
      reducer: (x, y) => y,
    }),
    tableConditions: Annotation<TableConditions>({
      reducer: (x, y) => y,
      default: () => null,
    }),
    dateCondition: Annotation<DateCondition>({
      reducer: (x, y) => y,
      default: () => null,
    }),
    questionConditions: Annotation<QuestionConditions>({
      reducer: (x, y) => y,
      default: () => null,
    }),
    query: Annotation<Query>({
      reducer: (x, y) => y,
    }),
  });

  const DatabaseAgentState = Annotation.Root({
    ...OverallStateAnnotation.spec,
    ...QueryBuilderState.spec,
  });

  const flattenJsonTablesInSchema = async (
    state: typeof DatabaseAgentState.State
  ) => {
    // feature flag for JSON Flattening
    // Set to true to disable JSON flattening
    if (true) {
      return { schema: state.schema, jsonColumnSchemas: null };
    }

    const tablesWithJsonToFlatten = state.schema.filter((table) =>
      table.columns.some((column) => column.type === "Json")
    );

    if (tablesWithJsonToFlatten.length === 0) {
      return { tablesWithJsonToFlatten: [] };
    }

    let jsonColumns: string[] = [];
    tablesWithJsonToFlatten.forEach((table) => {
      jsonColumns = table.columns
        .filter((column) => column.type === "Json")
        .map((column) => column.name);
      if (jsonColumns.length > 1) {
        throw new Error(
          `Not Supported: Table ${table.name} has more than one JSON column`
        );
      }
    });

    const userDatabaseConnector = new UserDatabaseConnector({
      baseURL: params.connectorUrl,
      signingSecret: params.connectorSigningKey,
    });

    const jsonColumnSchemas = await Promise.all(
      tablesWithJsonToFlatten.map(async (table) => {
        const orderByColumn = table.columns.find(
          (column) => column.type === "Int" || column.type === "String"
        )?.name;
        const jsonColumn = table.columns.find(
          (column) => column.type === "Json"
        );
        const q = {
          table: table.name,
          operation: "findMany",
          operationParameters: {
            columns: {
              [table.name]: [jsonColumn?.name],
            },
            orderBy: { column: orderByColumn, direction: "asc" },
            limit: 1,
          },
          computedColumns: [],
        };
        const tableConditions = buildConditionsForTable(
          table,
          params.requestContext
        );

        const queryWithConditions = formatAllConditions(
          q,
          tableConditions,
          null,
          null
        );
        const parsedQuery = querySchema.parse(queryWithConditions);
        const { data: jsonColumnExampleValues } =
          await userDatabaseConnector.query(parsedQuery);
        if (
          !Array.isArray(jsonColumnExampleValues) ||
          jsonColumnExampleValues.length === 0
        ) {
          throw new Error(
            `No JSON column example values found for table ${table.name}`
          );
        }
        const jsonColumnExample = jsonColumnExampleValues?.[0] as Record<
          string,
          Record<string, string | number>
        >;
        return mapJsonToSchema(jsonColumnExample, table.name);
      })
    );

    const schemaWithJsonColumns = state.schema.map((table) => {
      const jsonColumnSchema = jsonColumnSchemas.find(
        (schema) => schema.tableName === table.name
      );
      if (!jsonColumnSchema) {
        return table;
      }
      return {
        ...table,
        columns: [...table.columns, ...jsonColumnSchema.jsonSchema],
      };
    });
    return {
      schema: schemaWithJsonColumns,
      jsonColumnSchema: jsonColumnSchemas,
    };
  };

  const selectTableName = async (state: typeof DatabaseAgentState.State) => {
    const model = getAIModel("azure:gpt-4.1");
    const selectTablePrompt = await getPrompt("select_table:90d4f02c");
    const tableNames = state.schema
      .filter((table) => table.access === "QUERYABLE")
      .map((table) => table.name);
    const tableSelector = model.withStructuredOutput(
      z.object({
        table: stringArrayToZodEnum(tableNames).describe("The table to query"),
      }),
      {
        method: "jsonSchema",
        strict: true,
      }
    );
    const response = await selectTablePrompt.pipe(tableSelector).invoke({
      schema: buildFullSchemaString(state.schema),
      question: state.question,
    });
    const tableSchema = state.schema.find((t) => t.name === response.table);
    assert(tableSchema, `Table '${response.table}' not found`);
    return { tableName: response.table, tableSchema };
  };

  const setContextFilters = async (state: typeof DatabaseAgentState.State) => {
    const conditions = buildConditionsForTable(
      state.tableSchema,
      params.requestContext
    );
    return { tableConditions: conditions };
  };

  const selectDatabaseOperation = async (
    state: typeof DatabaseAgentState.State
  ) => {
    const model = getAIModel("azure:gpt-4.1");
    const operationSelectorPrompt = await getPrompt(
      "select_operation:d53071cb"
    );

    const { columns = [], outwardRelations: relations = [] } =
      state.tableSchema;
    assert(columns.length > 0, "Table has no columns");

    const ops: string[] = [];
    const docs: Record<string, unknown> = {};

    const tableHasOneToManyRelations =
      relations.filter((relation) => relation.isList).length >= 1;

    for (const [key, value] of Object.entries(operationDocs)) {
      if ("requiresOneToManyRelation" in value && !tableHasOneToManyRelations) {
        continue;
      }
      docs[key] = value;
      ops.push(key);
    }

    const operationSelector = model.withStructuredOutput(
      z.object({
        operation: stringArrayToZodEnum(ops).describe(
          "The database operation to perform. Choose NONE if no suitable operation is available"
        ),
      }),
      {
        method: "jsonSchema",
        strict: true,
      }
    );

    const operationResponse = await operationSelectorPrompt
      .pipe(operationSelector)
      .invoke({
        user_question: state.question,
        operationDocs: YAML.stringify(operationDocs, null, 2),
        filterDocsSummary: whereConditionDocsSummary,
        tableSchema: buildTableSchemaStringFromTableSchema(state.tableSchema),
        tableConditions: JSON.stringify(state.tableConditions),
      });
    if (operationResponse.operation === "NONE") {
      throw new Error(
        "Sorry, I was not able to find a suitable operation for your question. Please try rephrasing your question or providing more context."
      );
    }
    return { operation: operationResponse.operation };
  };

  const defineOperationParams = async (
    state: typeof DatabaseAgentState.State
  ) => {
    const operationParamsResponse = await operationParametersAgent({
      operation: state.operation,
      schema: state.schema,
      tableSchema: state.tableSchema,
      tableName: state.tableName,
      question: state.question,
    }).invoke({});
    return { operationParams: operationParamsResponse.operationParameters };
  };

  const setMessageDerivedFilters = async (
    state: typeof DatabaseAgentState.State
  ) => {
    const questionWhereAgentResponse = await questionWhereConditionAgent({
      schema: state.schema,
      tableSchema: state.tableSchema,
      tableName: state.tableName,
      operation: state.operation,
      operationParams: state.operationParams,
      question: state.question,
      dateCondition: state.dateCondition,
      tableConditions: state.tableConditions,
      requestContext: params.requestContext,
      connectorUrl: params.connectorUrl,
      connectorSigningKey: params.connectorSigningKey,
    });
    return { questionConditions: questionWhereAgentResponse };
  };

  const buildQuery = async (state: typeof DatabaseAgentState.State) => {
    const query: DBQuery = {
      table: state.tableName,
      operation: state.operation,
      operationParameters: state.operationParams,
    };

    const columnLookup = buildColumnLookup(state.schema);
    // Tracker pass by ref and updated in the normalization functions
    const columnAliasTracker: ColumnAliasMap = {};

    const normalizedTableConditions = normalizeTableConditions(
      state.tableConditions,
      state.tableName,
      columnLookup,
      columnAliasTracker
    );
    const normalizedQuestionConditions = normalizeQuestionConditions(
      state.questionConditions,
      state.tableName,
      state.schema,
      columnLookup,
      columnAliasTracker
    );
    const normalizedDateCondition = normalizeDateCondition(
      state.dateCondition,
      state.tableName,
      columnLookup,
      columnAliasTracker
    );

    const queryWithConditions = formatAllConditions(
      query,
      normalizedTableConditions,
      normalizedQuestionConditions,
      normalizedDateCondition
    );

    const relatedTableNames = new Set([
      state.tableName,
      ...state.tableSchema.outwardRelations.map((r) => r.targetTable.name),
    ]);

    const computedColumns = state.schema
      .filter((t) => relatedTableNames.has(t.name) && t.computedColumns?.length)
      .flatMap((t) => t.computedColumns);

    if (computedColumns.length > 0) {
      queryWithConditions.computedColumns = computedColumns;
    }

    if (state.jsonColumnSchema && state.jsonColumnSchema.length > 0) {
      queryWithConditions.jsonColumnSchema = state.jsonColumnSchema;
    }

    const queryUsingDbNames = normalizeQueryColumnReferences(
      queryWithConditions,
      state.schema,
      state.tableName,
      columnLookup,
      columnAliasTracker
    );

    const parsedQuery = querySchema.parse(queryUsingDbNames);

    const columnAliasMap = Object.entries(
      columnAliasTracker
    ).reduce<ColumnAliasMap>((acc, [table, entries]) => {
      if (entries.length > 0) {
        acc[table] = entries;
      }
      return acc;
    }, {});

    return { query: parsedQuery, columnAliasMap };
  };

  const executeQuery = async (state: typeof DatabaseAgentState.State) => {
    const userDatabaseConnector = new UserDatabaseConnector({
      baseURL: params.connectorUrl,
      signingSecret: params.connectorSigningKey,
    });
    const queryResponse = await userDatabaseConnector.query(state.query);
    return { queryResponse: queryResponse };
  };

  const formatDatabaseResponse = async (
    state: typeof DatabaseAgentState.State
  ) => {
    return {
      databaseResponse: {
        query: state.queryResponse?.query,
        response: state.queryResponse?.data,
      },
    };
  };

  const workflow = new StateGraph(OverallStateAnnotation)
    .addNode("flatten_json_tables", flattenJsonTablesInSchema)
    .addNode("select_table_name", selectTableName, {
      input: DatabaseAgentState,
      metadata: {
        userObservable: true,
      },
    })
    .addNode("set_context_filters", setContextFilters, {
      input: DatabaseAgentState,
      metadata: {
        userObservable: true,
      },
    })
    .addNode("select_operation", selectDatabaseOperation, {
      input: DatabaseAgentState,
      metadata: {
        userObservable: true,
      },
    })
    .addNode("define_operation_params", defineOperationParams, {
      input: DatabaseAgentState,
      metadata: {
        userObservable: true,
      },
    })
    .addNode("set_message_derived_filters", setMessageDerivedFilters, {
      input: DatabaseAgentState,
      metadata: {
        userObservable: true,
      },
    })
    .addNode("build_query", buildQuery, {
      input: DatabaseAgentState,
      metadata: {
        userObservable: true,
      },
    })
    .addNode("execute_query", executeQuery, {
      input: DatabaseAgentState,
      metadata: {
        userObservable: true,
      },
    })
    .addNode("format_database_response", formatDatabaseResponse, {
      input: DatabaseAgentState,
    })
    .addEdge(START, "flatten_json_tables")
    .addEdge("flatten_json_tables", "select_table_name")
    .addEdge("select_table_name", "set_context_filters")
    .addEdge("set_context_filters", "select_operation")
    .addEdge("select_operation", "define_operation_params")
    .addEdge("define_operation_params", "set_message_derived_filters")
    .addEdge("set_message_derived_filters", "build_query")
    .addEdge("build_query", "execute_query")
    .addEdge("execute_query", "format_database_response")
    .addEdge("format_database_response", END);

  return workflow.compile();
}
