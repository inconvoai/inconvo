import { Annotation, END, START, StateGraph } from "@langchain/langgraph";
import {
  querySchema,
  whereAndArraySchema,
  type DatabaseConnector,
  type Query,
  type QueryResponse,
  type QuestionConditions,
  type Schema,
} from "@repo/types";
import { ZodError } from "zod";
import {
  type DatabaseRetrieverQueryDraft,
  type SupportedDatabaseRetrieverOperation,
} from "./queryDraft";
import {
  type OperationParametersValidationIssue,
  validateOperationParametersDraft,
} from "./operationParameters/validateOperationParametersDraft";
import { validateQuestionConditions } from "./questionWhere/dynamicSchema";
import type { DBQuery } from "./types";
import { extractJoinedTableNames } from "./utils/extractJoinedTableNames";
import { generatePrismaWhereArray } from "./utils/generatePrismaWhereArray";
import {
  buildConditionsForTable,
  buildTableConditionsMap,
} from "./utils/buildConditionsForTable";
import { generateJoinGraph } from "./utils/tableRelations";

interface RequestParams {
  query: DatabaseRetrieverQueryDraft;
  schema: Schema;
  userContext: Record<string, string | number | boolean>;
  connector: DatabaseConnector;
}

export type DatabaseRetrieverError = {
  type: "validation" | "execution";
  stage:
    | "table"
    | "operation"
    | "operationParameters"
    | "questionConditions"
    | "query"
    | "execution";
  issues?: OperationParametersValidationIssue[];
  message?: string;
};

// Context condition for RLS/tenant filtering
export type ContextCondition = {
  table: string;
  column: string; // semantic column name
  operator: "equals";
  value: string | number | boolean;
};

function extractTableFromQualifiedColumn(
  qualifiedColumn: string,
): string | null {
  const lastDot = qualifiedColumn.lastIndexOf(".");
  if (lastDot === -1) return null;
  return qualifiedColumn.slice(0, lastDot);
}

function normalizeIssuesFromZodError(
  error: ZodError,
): OperationParametersValidationIssue[] {
  return error.issues.map((issue) => ({
    path: issue.path.join(".") || "<root>",
    message: issue.message,
    code: issue.code,
  }));
}

function createValidationError(
  stage: DatabaseRetrieverError["stage"],
  options: {
    issues?: OperationParametersValidationIssue[];
    message?: string;
  } = {},
): DatabaseRetrieverError {
  return {
    type: "validation",
    stage,
    message:
      options.message ??
      buildValidationErrorMessage(stage),
    ...(options.issues ? { issues: options.issues } : {}),
  };
}

function createExecutionError(
  message: string,
  details?: Record<string, unknown>,
): DatabaseRetrieverError {
  return {
    type: "execution",
    stage: "execution",
    message,
    ...(details && Object.keys(details).length > 0
      ? {
          issues: Object.entries(details).map(([path, value]) => ({
            path,
            code: "execution_detail",
            message:
              typeof value === "string" ? value : JSON.stringify(value, null, 2),
          })),
        }
      : {}),
  };
}

function logRetrieverEvent(
  event: string,
  payload: Record<string, unknown>,
): void {
  console.info(
    JSON.stringify({
      event,
      component: "databaseRetriever",
      ...payload,
    }),
  );
}

function buildValidationErrorMessage(
  stage: DatabaseRetrieverError["stage"],
): string {
  switch (stage) {
    case "table":
      return "Invalid base table selection.";
    case "operation":
      return "Invalid operation for the selected base table.";
    case "operationParameters":
      return "Invalid operation parameters.";
    case "questionConditions":
      return "Invalid question filters.";
    case "query":
      return "Structured query draft could not be built.";
    case "execution":
      return "The database query failed during execution.";
    default:
      return "Invalid structured query draft.";
  }
}

export function assertQueryReferencesAllowedTables(
  query: Query,
  schema: Schema,
): void {
  const allowedTables = new Set(schema.map((table) => table.name));
  const referencedTables = new Set<string>([query.table]);

  const operationParams = query.operationParameters as
    | {
        joins?: Array<{
          table?: unknown;
          path?: Array<{
            source?: unknown;
            target?: unknown;
          }>;
        }> | null;
      }
    | null
    | undefined;

  const joins = operationParams?.joins;
  if (Array.isArray(joins)) {
    for (const join of joins) {
      if (typeof join?.table === "string") {
        referencedTables.add(join.table);
      }

      const path = join?.path;
      if (!Array.isArray(path)) continue;

      for (const hop of path) {
        if (Array.isArray(hop?.source)) {
          for (const sourceColumn of hop.source) {
            if (typeof sourceColumn !== "string") continue;
            const tableName = extractTableFromQualifiedColumn(sourceColumn);
            if (tableName) {
              referencedTables.add(tableName);
            }
          }
        }
        if (Array.isArray(hop?.target)) {
          for (const targetColumn of hop.target) {
            if (typeof targetColumn !== "string") continue;
            const tableName = extractTableFromQualifiedColumn(targetColumn);
            if (tableName) {
              referencedTables.add(tableName);
            }
          }
        }
      }
    }
  }

  const disallowed = Array.from(referencedTables).filter(
    (tableName) => !allowedTables.has(tableName),
  );

  if (disallowed.length > 0) {
    throw new Error(
      `Query references tables not available in runtime schema: ${disallowed.join(", ")}`,
    );
  }
}

export async function databaseRetrieverAgent(params: RequestParams) {
  const retrieverStartedAt = Date.now();

  const DatabaseAgentState = Annotation.Root({
    draft: Annotation<DatabaseRetrieverQueryDraft>({
      reducer: (_x, y) => y,
      default: () => params.query,
    }),
    schema: Annotation<Schema>({
      reducer: (_x, y) => y,
      default: () => params.schema,
    }),
    tableName: Annotation<string>({
      reducer: (_x, y) => y,
    }),
    tableSchema: Annotation<Schema[number]>({
      reducer: (_x, y) => y,
    }),
    operation: Annotation<SupportedDatabaseRetrieverOperation>({
      reducer: (_x, y) => y,
    }),
    operationParameters: Annotation<Record<string, unknown>>({
      reducer: (_x, y) => y,
    }),
    joinedTableNames: Annotation<string[]>({
      reducer: (_x, y) => y,
      default: () => [],
    }),
    joinedScalarFilterTargets: Annotation<Record<string, string>>({
      reducer: (_x, y) => y,
      default: () => ({}),
    }),
    contextConditions: Annotation<ContextCondition[]>({
      reducer: (_x, y) => y,
      default: () => [],
    }),
    questionConditions: Annotation<QuestionConditions>({
      reducer: (_x, y) => y,
      default: () => null,
    }),
    preExecutionMs: Annotation<number | undefined>({
      reducer: (_x, y) => y,
      default: () => undefined,
    }),
    query: Annotation<Query>({
      reducer: (_x, y) => y,
    }),
    queryResponse: Annotation<QueryResponse | null>({
      reducer: (_x, y) => y,
      default: () => null,
    }),
    databaseResponse: Annotation<{
      query: QueryResponse["query"];
      response: unknown;
      warning?: string;
    }>({
      reducer: (_x, y) => y,
    }),
    error: Annotation<DatabaseRetrieverError | undefined>({
      reducer: (_x, y) => y,
      default: () => undefined,
    }),
  });

  const routeOnError = (next: string) => (state: typeof DatabaseAgentState.State) =>
    state.error ? "end" : next;

  const resolveTable = async (state: typeof DatabaseAgentState.State) => {
    const tableSchema = state.schema.find(
      (table) => table.name === state.draft.table,
    );

    if (!tableSchema) {
      const error = createValidationError("table", {
        issues: [
          {
            path: "table",
            code: "unknown_table",
            message: `Table "${state.draft.table}" was not found in the runtime schema.`,
          },
        ],
      });
      logRetrieverEvent("validation_error", {
        stage: error.stage,
        validation_stage: error.stage,
        validation_issue_count: error.issues?.length ?? 0,
      });
      return { error };
    }

    if (tableSchema.access !== "QUERYABLE") {
      const error = createValidationError("table", {
        issues: [
          {
            path: "table",
            code: "table_not_queryable",
            message: `Table "${state.draft.table}" cannot be used as a base query table.`,
          },
        ],
      });
      logRetrieverEvent("validation_error", {
        stage: error.stage,
        validation_stage: error.stage,
        validation_issue_count: error.issues?.length ?? 0,
      });
      return { error };
    }

    return {
      tableName: tableSchema.name,
      tableSchema,
    };
  };

  const validateOperation = async (state: typeof DatabaseAgentState.State) => {
    const operation = state.draft.operation;

    if (
      operation === "countRelations" &&
      !(state.tableSchema.outwardRelations ?? []).some((relation) => relation.isList)
    ) {
      const error = createValidationError("operation", {
        issues: [
          {
            path: "operation",
            code: "requires_to_many_relation",
            message:
              "countRelations requires a base table with at least one to-many relation.",
          },
        ],
      });
      logRetrieverEvent("validation_error", {
        stage: error.stage,
        validation_stage: error.stage,
        validation_issue_count: error.issues?.length ?? 0,
      });
      return { error };
    }

    return { operation };
  };

  const validateOperationParams = async (
    state: typeof DatabaseAgentState.State,
  ) => {
    const validation = validateOperationParametersDraft({
      operation: state.operation,
      candidateOperationParameters: state.draft.operationParameters,
      schema: state.schema,
      tableName: state.tableName,
      tableSchema: state.tableSchema,
    });

    if (validation.status === "invalid") {
      const error = createValidationError("operationParameters", {
        issues: validation.issues,
      });
      logRetrieverEvent("validation_error", {
        stage: error.stage,
        validation_stage: error.stage,
        validation_issue_count: error.issues?.length ?? 0,
      });
      return { error };
    }

    return {
      operationParameters: validation.operationParameters,
    };
  };

  const deriveJoinedTables = async (state: typeof DatabaseAgentState.State) => {
    const joins = state.operationParameters.joins as
      | Array<{
          table: string;
          name?: string;
          path: Array<{ source: string[]; target: string[] }>;
        }>
      | null
      | undefined;

    const joinedScalarFilterTargets = Object.fromEntries(
      (joins ?? []).map((join) => [join.name ?? join.table, join.table]),
    );

    return {
      joinedTableNames: extractJoinedTableNames(joins).filter(
        (tableName) => tableName !== state.tableName,
      ),
      joinedScalarFilterTargets,
    };
  };

  const setContextFilters = async (state: typeof DatabaseAgentState.State) => {
    try {
      const contextConditions: ContextCondition[] = [];

      const baseTableConditions = buildConditionsForTable(
        state.tableSchema,
        params.userContext,
      );

      if (baseTableConditions) {
        for (const condition of baseTableConditions) {
          contextConditions.push({
            table: state.tableName,
            column: condition.column,
            operator: "equals",
            value: condition.value as string | number | boolean,
          });
        }
      }

      for (const joinedTableName of state.joinedTableNames) {
        const joinedTableSchema = state.schema.find(
          (table) => table.name === joinedTableName,
        );
        if (!joinedTableSchema) {
          continue;
        }

        const joinedTableConditions = buildConditionsForTable(
          joinedTableSchema,
          params.userContext,
        );

        if (joinedTableConditions) {
          for (const condition of joinedTableConditions) {
            contextConditions.push({
              table: joinedTableName,
              column: condition.column,
              operator: "equals",
              value: condition.value as string | number | boolean,
            });
          }
        }
      }

      return { contextConditions };
    } catch (error) {
      const validationError = createValidationError("query", {
        message: error instanceof Error ? error.message : String(error),
      });
      logRetrieverEvent("validation_error", {
        stage: validationError.stage,
        validation_stage: validationError.stage,
        validation_issue_count: validationError.issues?.length ?? 0,
      });
      return { error: validationError };
    }
  };

  const validateQuestionConditionsNode = async (
    state: typeof DatabaseAgentState.State,
  ) => {
    const validation = validateQuestionConditions(
      state.draft.questionConditions,
      state.tableSchema,
      state.schema,
      state.tableName,
      state.joinedScalarFilterTargets,
    );

    if (!validation.success) {
      const error = createValidationError("questionConditions", {
        issues: normalizeIssuesFromZodError(validation.error),
      });
      logRetrieverEvent("validation_error", {
        stage: error.stage,
        validation_stage: error.stage,
        validation_issue_count: error.issues?.length ?? 0,
      });
      return { error };
    }

    return {
      questionConditions: validation.data,
    };
  };

  const buildQuery = async (state: typeof DatabaseAgentState.State) => {
    try {
      const query: DBQuery = {
        table: state.tableName,
        tableSchema: state.tableSchema.schema ?? null,
        operation: state.operation,
        operationParameters: state.operationParameters,
        tableConditions: null,
      };

      const { uniqueTableNames } = generateJoinGraph(
        state.schema,
        state.tableName,
        2,
      );
      const tableConditionsMap = buildTableConditionsMap(
        state.schema,
        uniqueTableNames,
        params.userContext,
      );

      const whereAndArray = generatePrismaWhereArray(
        state.contextConditions,
        state.questionConditions,
      );

      query.whereAndArray = whereAndArraySchema.parse(whereAndArray);
      query.tableConditions = tableConditionsMap ?? null;

      const parsedQuery = querySchema.parse(query);
      const preExecutionMs = Date.now() - retrieverStartedAt;

      logRetrieverEvent("pre_execution_ready", {
        databaseRetriever_pre_execution_ms: preExecutionMs,
        joined_table_count: state.joinedTableNames.length,
        context_condition_count: state.contextConditions.length,
      });

      return { query: parsedQuery, preExecutionMs };
    } catch (error) {
      const validationError =
        error instanceof ZodError
          ? createValidationError("query", {
              issues: normalizeIssuesFromZodError(error),
            })
          : createValidationError("query", {
              message: error instanceof Error ? error.message : String(error),
            });
      logRetrieverEvent("validation_error", {
        stage: validationError.stage,
        validation_stage: validationError.stage,
        validation_issue_count: validationError.issues?.length ?? 0,
      });
      return { error: validationError };
    }
  };

  const executeQuery = async (state: typeof DatabaseAgentState.State) => {
    const executionStartedAt = Date.now();

    try {
      assertQueryReferencesAllowedTables(state.query, state.schema);
      const queryResponse = await params.connector.query(state.query);
      logRetrieverEvent("execution_success", {
        connector_execution_ms: Date.now() - executionStartedAt,
        databaseRetriever_pre_execution_ms:
          state.preExecutionMs ?? executionStartedAt - retrieverStartedAt,
        joined_table_count: state.joinedTableNames.length,
        context_condition_count: state.contextConditions.length,
      });
      return { queryResponse };
    } catch (error) {
      if (
        error instanceof Error &&
        error.name === "ConnectQueryError" &&
        "details" in error
      ) {
        const details = (error as { details: Record<string, unknown> }).details;
        const executionError = createExecutionError(
          error.message,
          details,
        );
        logRetrieverEvent("execution_error", {
          stage: executionError.stage,
          validation_issue_count: executionError.issues?.length ?? 0,
          connector_execution_ms: Date.now() - executionStartedAt,
        });
        return { error: executionError };
      }

      const executionError = createExecutionError(
        error instanceof Error ? error.message : String(error),
      );
      logRetrieverEvent("execution_error", {
        stage: executionError.stage,
        validation_issue_count: executionError.issues?.length ?? 0,
        connector_execution_ms: Date.now() - executionStartedAt,
      });
      return { error: executionError };
    }
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
        query: state.queryResponse!.query,
        response,
        ...(warning ? { warning } : {}),
      },
    };
  };

  const workflow = new StateGraph(DatabaseAgentState)
    .addNode("resolve_table", resolveTable, {
      metadata: { userObservable: true },
    })
    .addNode("validate_operation", validateOperation, {
      metadata: { userObservable: true },
    })
    .addNode("validate_operation_params", validateOperationParams, {
      metadata: { userObservable: true },
    })
    .addNode("derive_joined_tables", deriveJoinedTables, {
      metadata: { userObservable: true },
    })
    .addNode("set_context_filters", setContextFilters, {
      metadata: { userObservable: true },
    })
    .addNode("validate_question_conditions", validateQuestionConditionsNode, {
      metadata: { userObservable: true },
    })
    .addNode("build_query", buildQuery, {
      metadata: { userObservable: true },
    })
    .addNode("execute_query", executeQuery, {
      metadata: { userObservable: true },
    })
    .addNode("format_database_response", formatDatabaseResponse)
    .addEdge(START, "resolve_table")
    .addConditionalEdges("resolve_table", routeOnError("validate_operation"), {
      end: END,
      validate_operation: "validate_operation",
    })
    .addConditionalEdges(
      "validate_operation",
      routeOnError("validate_operation_params"),
      {
        end: END,
        validate_operation_params: "validate_operation_params",
      },
    )
    .addConditionalEdges(
      "validate_operation_params",
      routeOnError("derive_joined_tables"),
      {
        end: END,
        derive_joined_tables: "derive_joined_tables",
      },
    )
    .addEdge("derive_joined_tables", "set_context_filters")
    .addConditionalEdges(
      "set_context_filters",
      routeOnError("validate_question_conditions"),
      {
        end: END,
        validate_question_conditions: "validate_question_conditions",
      },
    )
    .addConditionalEdges(
      "validate_question_conditions",
      routeOnError("build_query"),
      {
        end: END,
        build_query: "build_query",
      },
    )
    .addConditionalEdges("build_query", routeOnError("execute_query"), {
      end: END,
      execute_query: "execute_query",
    })
    .addConditionalEdges("execute_query", routeOnError("format_database_response"), {
      end: END,
      format_database_response: "format_database_response",
    })
    .addEdge("format_database_response", END);

  return workflow.compile();
}
