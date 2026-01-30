import { z } from "zod";
import type { DatabaseConnector, Query, QueryResponse } from "@repo/types";
import { getAIModel, type AIProvider } from "../utils/getAIModel";
import assert from "assert";
import { Annotation, END, START, StateGraph } from "@langchain/langgraph";
import { querySchema, whereAndArraySchema } from "@repo/types";
import { generatePrismaWhereArray } from "./utils/generatePrismaWhereArray";
import { operationDocs } from "./utils/operationDocs";
import { getPrompt } from "../utils/getPrompt";
import YAML from "yaml";
import { questionWhereConditionAgent } from "./questionWhere";
import type { DBQuery, Operation } from "./types";
import type { QuestionConditions } from "@repo/types";
import { stringArrayToZodEnum } from "../utils/zodHelpers";
import {
  buildFullSchemaString,
  buildTableSchemaStringFromTableSchema,
} from "./utils/schemaFormatters";
import { operationParametersAgent } from "./operationParameters";
import type { Schema } from "@repo/types";
import {
  buildConditionsForTable,
  buildTableConditionsMap,
} from "./utils/buildConditionsForTable";
import { generateJoinGraph } from "./utils/tableRelations";
import { extractJoinedTableNames } from "./utils/extractJoinedTableNames";
import { whereConditionDocsSummary } from "./utils/whereDocs";
import { buildPromptCacheKey } from "../utils/promptCacheKey";
import {
  buildColumnLookup,
  canonicalizeQueryColumnReferences,
  canonicalizeQuestionConditions,
} from "./utils/queryCanonicalization";
import type { ColumnAliasMap } from "./utils/queryCanonicalization";

interface RequestParams {
  userQuestion: string;
  schema: Schema;
  userContext: Record<string, string | number>;
  connector: DatabaseConnector;
  agentId: string | number;
  provider: AIProvider;
}

// Context condition for RLS/tenant filtering
export type ContextCondition = {
  table: string;
  column: string; // semantic column name
  operator: "equals";
  value: string | number;
};

export async function databaseRetrieverAgent(params: RequestParams) {
  const promptCacheKey = buildPromptCacheKey({
    agentId: params.agentId,
    userContext: params.userContext,
  });
  const OverallStateAnnotation = Annotation.Root({
    question: Annotation<string>({
      reducer: (x, y) => y,
      default: () => params.userQuestion,
    }),
    schema: Annotation<Schema>({
      reducer: (x, y) => y,
      default: () => params.schema,
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
      warning?: string;
    }>({
      reducer: (x, y) => y,
    }),
    columnAliasMap: Annotation<ColumnAliasMap>({
      reducer: (x, y) => y,
      default: () => ({}) as ColumnAliasMap,
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
    joinedTableNames: Annotation<string[]>({
      reducer: (x, y) => y,
      default: () => [],
    }),
    contextConditions: Annotation<ContextCondition[]>({
      reducer: (x, y) => y,
      default: () => [],
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

  const selectTableName = async (state: typeof DatabaseAgentState.State) => {
    const model = getAIModel(params.provider, "gpt-5.1", {
      promptCacheKey,
    });
    const selectTablePrompt = await getPrompt("select_table:dbe22856");
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
      },
    );
    const response = await selectTablePrompt.pipe(tableSelector).invoke({
      schema: buildFullSchemaString(state.schema),
      question: state.question,
    });
    const tableSchema = state.schema.find((t) => t.name === response.table);
    assert(tableSchema, `Table '${response.table}' not found`);
    return { tableName: response.table, tableSchema };
  };

  const selectDatabaseOperation = async (
    state: typeof DatabaseAgentState.State,
  ) => {
    const model = getAIModel(params.provider, "gpt-5.2", {
      promptCacheKey,
    });
    const operationSelectorPrompt = await getPrompt(
      "select_operation:e74c537d",
    );

    const columns = state.tableSchema.columns;
    const relations = state.tableSchema.outwardRelations ?? [];
    assert(columns.length > 0, "Table has no columns");

    const ops: string[] = [];
    const docs: Record<string, unknown> = {};

    const tableHasOneToManyRelations = relations.some(
      (relation: Schema[number]["outwardRelations"][number]) => relation.isList,
    );

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
          "The database operation to perform. Choose NONE if no suitable operation is available",
        ),
      }),
      {
        method: "jsonSchema",
        strict: true,
      },
    );

    // Build table conditions inline for prompt context
    const tableConditionsForPrompt = buildConditionsForTable(
      state.tableSchema,
      params.userContext,
    );

    const operationResponse = await operationSelectorPrompt
      .pipe(operationSelector)
      .invoke({
        user_question: state.question,
        operationDocs: YAML.stringify(operationDocs, null, 2),
        filterDocsSummary: whereConditionDocsSummary,
        tableSchema: buildTableSchemaStringFromTableSchema(state.tableSchema),
        tableConditions: JSON.stringify(tableConditionsForPrompt),
      });
    if (operationResponse.operation === "NONE") {
      throw new Error(
        "Sorry, I was not able to find a suitable operation for your question. Please try rephrasing your question or providing more context.",
      );
    }
    return { operation: operationResponse.operation };
  };

  const defineOperationParams = async (
    state: typeof DatabaseAgentState.State,
  ) => {
    const operationParamsResponse = await operationParametersAgent({
      operation: state.operation,
      schema: state.schema,
      tableSchema: state.tableSchema,
      tableName: state.tableName,
      question: state.question,
      userContext: params.userContext,
      agentId: params.agentId,
      provider: params.provider,
    }).invoke({});

    const operationParams = operationParamsResponse.operationParameters;

    // Extract joined table names from operation params
    const joins = operationParams?.joins as
      | Array<{
          table: string;
          path: Array<{ source: string[]; target: string[] }>;
        }>
      | null
      | undefined;
    const joinedTableNames = extractJoinedTableNames(joins);

    return { operationParams, joinedTableNames };
  };

  const setContextFilters = async (state: typeof DatabaseAgentState.State) => {
    const contextConditions: ContextCondition[] = [];

    // Build conditions for the base table
    const baseTableConditions = buildConditionsForTable(
      state.tableSchema,
      params.userContext,
    );

    if (baseTableConditions) {
      for (const cond of baseTableConditions) {
        contextConditions.push({
          table: state.tableName,
          column: cond.column,
          operator: "equals",
          value: cond.value as string | number,
        });
      }
    }

    // Build conditions for joined tables (using joinedTableNames from state)
    for (const joinedTableName of state.joinedTableNames) {
      const joinedTableSchema = state.schema.find(
        (t) => t.name === joinedTableName,
      );
      if (!joinedTableSchema) continue;

      const joinedTableConditions = buildConditionsForTable(
        joinedTableSchema,
        params.userContext,
      );
      if (joinedTableConditions) {
        for (const cond of joinedTableConditions) {
          contextConditions.push({
            table: joinedTableName,
            column: cond.column,
            operator: "equals",
            value: cond.value as string | number,
          });
        }
      }
    }

    return { contextConditions };
  };

  const setMessageDerivedFilters = async (
    state: typeof DatabaseAgentState.State,
  ) => {
    const questionWhereAgentResponse = await questionWhereConditionAgent({
      schema: state.schema,
      tableSchema: state.tableSchema,
      tableName: state.tableName,
      operation: state.operation,
      operationParams: state.operationParams,
      question: state.question,
      contextConditions: state.contextConditions,
      joinedTableNames: state.joinedTableNames,
      userContext: params.userContext,
      agentId: params.agentId,
      provider: params.provider,
    });
    return { questionConditions: questionWhereAgentResponse };
  };

  const buildQuery = async (state: typeof DatabaseAgentState.State) => {
    const query: DBQuery = {
      table: state.tableName,
      tableSchema: state.tableSchema.schema ?? null,  // Include schema from selected table
      operation: state.operation,
      operationParameters: state.operationParams,
      tableConditions: null, // Set below after building tableConditionsMap
    };

    const columnLookup = buildColumnLookup(state.schema);
    // Tracker passed by ref and updated in the canonicalization functions
    const columnAliasTracker: ColumnAliasMap = {};

    // Canonicalize context conditions (convert semantic column names to db names)
    const canonicalizedContextConditions: ContextCondition[] =
      state.contextConditions.map((c) => {
        const canonicalColumn =
          columnLookup.get(c.table)?.get(c.column) ?? c.column;
        return {
          ...c,
          column: canonicalColumn,
        };
      });
    const canonicalizedQuestionConditions = canonicalizeQuestionConditions(
      state.questionConditions,
      state.tableName,
      state.schema,
      columnLookup,
      columnAliasTracker,
    );

    // Build tableConditions map for all relevant tables (for relation subquery filtering)
    const { uniqueTableNames } = generateJoinGraph(
      state.schema,
      state.tableName,
      2, // depth of 2 to cover most relation filters
    );
    const tableConditionsMap = buildTableConditionsMap(
      state.schema,
      uniqueTableNames,
      params.userContext,
    );

    // Build whereAndArray from context conditions and question conditions
    const whereAndArray = generatePrismaWhereArray(
      canonicalizedContextConditions,
      canonicalizedQuestionConditions,
    );

    query.whereAndArray = whereAndArraySchema.parse(whereAndArray);
    query.tableConditions = tableConditionsMap ?? null;

    const queryWithConditions = query;

    const queryUsingDbNames = canonicalizeQueryColumnReferences(
      queryWithConditions,
      state.tableName,
      columnLookup,
      columnAliasTracker,
    );

    const parsedQuery = querySchema.parse(queryUsingDbNames);

    const columnAliasMap = Object.entries(
      columnAliasTracker,
    ).reduce<ColumnAliasMap>((acc, [table, entries]) => {
      if (entries.length > 0) {
        acc[table] = entries;
      }
      return acc;
    }, {});

    return { query: parsedQuery, columnAliasMap };
  };

  const executeQuery = async (state: typeof DatabaseAgentState.State) => {
    const queryResponse = await params.connector.query(state.query);
    return { queryResponse: queryResponse };
  };

  const formatDatabaseResponse = async (
    state: typeof DatabaseAgentState.State,
  ) => {
    const response = state.queryResponse?.data;
    let warning: string | undefined;

    if (Array.isArray(response) && response.length > 999) {
      warning =
        `WARNING: Result size hit the 1,000-row limit.\n` +
        `Data may be truncated.\n` +
        `Refine your query (filters, date ranges) or aggregate in SQL (e.g. GROUP BY) to reduce rows returned`;
    }

    return {
      databaseResponse: {
        query: state.queryResponse?.query,
        response: response,
        ...(warning ? { warning } : {}),
      },
    };
  };

  const workflow = new StateGraph(OverallStateAnnotation)
    .addNode("select_table_name", selectTableName, {
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
    .addNode("set_context_filters", setContextFilters, {
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
    .addEdge(START, "select_table_name")
    .addEdge("select_table_name", "select_operation")
    .addEdge("select_operation", "define_operation_params")
    .addEdge("define_operation_params", "set_context_filters")
    .addEdge("set_context_filters", "set_message_derived_filters")
    .addEdge("set_message_derived_filters", "build_query")
    .addEdge("build_query", "execute_query")
    .addEdge("execute_query", "format_database_response")
    .addEdge("format_database_response", END);

  return workflow.compile();
}
