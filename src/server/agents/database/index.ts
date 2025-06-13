import { z } from "zod";
import { UserDatabaseConnector } from "~/server/userDatabaseConnector";
import type {
  JsonColumnSchema,
  Query,
  QueryResponse,
} from "~/server/userDatabaseConnector/types";
import { AzureChatOpenAI } from "@langchain/openai";
import assert from "assert";
import {
  Annotation,
  Command,
  END,
  START,
  StateGraph,
} from "@langchain/langgraph";
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

interface RequestParams {
  userQuestion: string;
  organisationName: string;
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
    databaseResponse: Annotation<Record<string, unknown>[]>({
      reducer: (x, y) => (x ? [...x, ...y] : y),
    }),
    databaseResponseString: Annotation<string[]>({
      reducer: (x, y) => (x ? [...x, ...y] : y),
    }),
    // The reason for the answer isn't sufficient
    reason: Annotation<string>({
      reducer: (x, y) => y,
    }),
    error: Annotation<Record<string, unknown>>({
      reducer: (x, y) => y,
    }),
    // The number of follow-up questions asked
    notCompleteCount: Annotation<number>({
      reducer: (x, y) => y,
      default: () => 0,
    }),
    // Next conditional node to go to
    next: Annotation<string>({
      reducer: (x, y) => y ?? x ?? END,
      default: () => END,
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

  const model = new AzureChatOpenAI({
    model: "gpt-4.1",
    deploymentName: "gpt-4.1",
    temperature: 0,
  });

  const generateFollowUpQuestion = async (
    state: typeof OverallStateAnnotation.State
  ) => {
    const followUpQuestion = await getPrompt("generate_follow_up_question");
    const responseFormat = model.withStructuredOutput(
      z.object({
        question: z
          .string()
          .describe("The question to ask the database retriever"),
      }),
      { strict: true }
    );
    return followUpQuestion.pipe(responseFormat).invoke({
      question: params.userQuestion,
      reason: state.reason,
      databaseAgentResponse: state.databaseResponseString
        .map((message, idx) => `${idx + 1}): ${message}`)
        .join("\n\n"),
    });
  };

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
    const selectTablePrompt = await getPrompt("select_table");
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

  const defineTableConditions = async (
    state: typeof DatabaseAgentState.State
  ) => {
    const conditions = buildConditionsForTable(
      state.tableSchema,
      params.requestContext
    );
    return { tableConditions: conditions };
  };

  const selectDatabaseOperation = async (
    state: typeof DatabaseAgentState.State
  ) => {
    const operationSelectorPrompt = await getPrompt("select_operation");

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
        tableSchema: buildTableSchemaStringFromTableSchema(state.tableSchema),
        tableConditions: state.tableConditions,
      });
    if (operationResponse.operation === "NONE") {
      return new Command({
        update: {
          error: {
            message:
              "Sorry, I do not have the tools needed to answer this particular question right now.",
          },
        },
        graph: Command.PARENT,
        goto: "output_formatter",
      });
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

  const questionWhereConditions = async (
    state: typeof DatabaseAgentState.State
  ) => {
    const questionAgentResponse = await questionWhereConditionAgent({
      schema: state.schema,
      tableSchema: state.tableSchema,
      tableName: state.tableName,
      operation: state.operation,
      operationParams: state.operationParams,
      question: state.question,
      dateCondition: state.dateCondition,
      tableConditions: state.tableConditions,
      organisationName: params.organisationName,
      requestContext: params.requestContext,
      connectorUrl: params.connectorUrl,
      connectorSigningKey: params.connectorSigningKey,
    });
    return { questionConditions: questionAgentResponse };
  };

  const buildQuery = async (state: typeof DatabaseAgentState.State) => {
    const query: DBQuery = {
      table: state.tableName,
      operation: state.operation,
      operationParameters: state.operationParams,
      computedColumns: [],
    };

    const queryWithConditions = formatAllConditions(
      query,
      state.tableConditions,
      state.questionConditions,
      state.dateCondition
    );

    const computedColumns = state.tableSchema.computedColumns ?? [];

    if (computedColumns.length > 0) {
      queryWithConditions.computedColumns = computedColumns;
    }

    if (state.jsonColumnSchema && state.jsonColumnSchema.length > 0) {
      queryWithConditions.jsonColumnSchema = state.jsonColumnSchema;
    }

    const parsedQuery = querySchema.parse(queryWithConditions);

    return { query: parsedQuery };
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
      databaseResponse: [
        {
          query: state.queryResponse?.query,
          response: state.queryResponse?.data,
        },
      ],
      databaseResponseString: [
        `Query:\n${JSON.stringify(
          state.queryResponse?.query,
          null,
          2
        )}\nResponse:\n${JSON.stringify(state.queryResponse?.data, null, 2)}`,
      ],
    };
  };

  const decideComplete = async (state: typeof DatabaseAgentState.State) => {
    const nextStepPrompt = await getPrompt("decide_database_next_step");
    const nextStepSchema = model.withStructuredOutput(
      z.object({
        reason: z
          .string()
          .describe("The thinking behind the next selecting the next step"),
        next: z
          .enum(["generate_follow_up_question", END])
          .describe("The next step to take"),
      }),
      { method: "jsonSchema", strict: true }
    );
    const response = await nextStepPrompt.pipe(nextStepSchema).invoke({
      question: params.userQuestion,
      requestContext: params.requestContext,
      database_response: state.databaseResponseString
        .map((message, idx) => `${idx + 1}): ${message}`)
        .join("\n\n"),
      date: new Date().toISOString(),
    });

    if (
      state.notCompleteCount >= 1 &&
      response.next === "generate_follow_up_question"
    ) {
      return { next: END, reason: "Too many follow-up questions" };
    }

    return {
      next: response.next,
      reason: response.reason,
      notComplete: state.notCompleteCount + 1,
    };
  };

  const workflow = new StateGraph(OverallStateAnnotation)
    .addNode("flatten_json_tables", flattenJsonTablesInSchema)
    .addNode("generate_follow_up_question", generateFollowUpQuestion)
    .addNode("select_table_name", selectTableName, {
      input: DatabaseAgentState,
    })
    .addNode("define_table_conditions", defineTableConditions, {
      input: DatabaseAgentState,
    })
    .addNode("select_operation", selectDatabaseOperation, {
      input: DatabaseAgentState,
    })
    .addNode("define_operation_params", defineOperationParams, {
      input: DatabaseAgentState,
    })
    .addNode("question_where_conditions", questionWhereConditions, {
      input: DatabaseAgentState,
    })
    .addNode("build_query", buildQuery, { input: DatabaseAgentState })
    .addNode("execute_query", executeQuery, { input: DatabaseAgentState })
    .addNode("format_database_response", formatDatabaseResponse, {
      input: DatabaseAgentState,
    })
    .addNode("decide_complete", decideComplete, { input: DatabaseAgentState })
    .addEdge(START, "flatten_json_tables")
    .addEdge("flatten_json_tables", "select_table_name")
    .addEdge("select_table_name", "define_table_conditions")
    .addEdge("define_table_conditions", "select_operation")
    .addEdge("select_operation", "define_operation_params")
    .addEdge("define_operation_params", "question_where_conditions")
    .addEdge("question_where_conditions", "build_query")
    .addEdge("build_query", "execute_query")
    .addEdge("execute_query", "format_database_response")
    .addEdge("format_database_response", "decide_complete")
    .addConditionalEdges(
      "decide_complete",
      (x: typeof OverallStateAnnotation.State) => x.next
    )
    .addEdge("generate_follow_up_question", "select_table_name");

  return workflow.compile();
}
