import {
  NUMERIC_LOGICAL_TYPES,
  isActiveEnumColumn,
  type Schema,
} from "@repo/types";
import type { SupportedDatabaseRetrieverOperation } from "../queryDraft";
import {
  generateJoinGraph,
  type GeneratedJoinOption,
} from "../utils/tableRelations";
import {
  validateAggregateCandidate,
  type AggregateColumnCatalog,
  type AggregateColumnMetadata,
  type AggregateInvalidIssue,
  type AggregateValidationResult,
  type AggregateValidatorContext,
} from "./aggregate/aggregateValidator";
import {
  validateAggregateGroupsCandidate,
  type AggregateGroupsInvalidResultIssue,
  type AggregateGroupsValidationResult,
  type AggregateGroupsValidatorContext,
} from "./aggregateGroups/aggregateGroupsValidator";
import {
  validateCountCandidate,
  type CountInvalidIssue,
  type CountValidationResult,
  type CountValidatorContext,
} from "./count/countValidator";
import {
  validateCountRelationsCandidate,
  type CountRelationsInvalidResultIssue,
  type CountRelationsValidationResult,
  type CountRelationsValidatorContext,
} from "./countRelations/countRelationsValidator";
import {
  validateFindDistinctCandidate,
  type FindDistinctInvalidIssue,
  type FindDistinctValidationResult,
  type FindDistinctValidatorContext,
} from "./findDistinct/findDistinctValidator";
import {
  validateFindManyCandidate,
  type FindManyInvalidResultIssue,
  type FindManyValidationResult,
  type FindManyValidatorContext,
} from "./findMany/findManyValidator";
import {
  validateGroupByCandidate,
  type GroupByInvalidResultIssue,
  type GroupByValidationResult,
  type GroupByValidatorContext,
} from "./groupBy/groupByValidator";

export type OperationParametersValidationIssue = {
  path: string;
  message: string;
  code: string;
};

type OperationParametersValidationSuccess = {
  status: "valid";
  operationParameters: Record<string, unknown>;
};

type OperationParametersValidationFailure = {
  status: "invalid";
  issues: OperationParametersValidationIssue[];
};

export type OperationParametersValidationResult =
  | OperationParametersValidationSuccess
  | OperationParametersValidationFailure;

interface ValidateOperationParametersDraftParams {
  operation: SupportedDatabaseRetrieverOperation;
  candidateOperationParameters: Record<string, unknown>;
  schema: Schema;
  tableName: string;
  tableSchema: Schema[number];
}

export function validateOperationParametersDraft(
  params: ValidateOperationParametersDraftParams,
): OperationParametersValidationResult {
  switch (params.operation) {
    case "findMany":
      return validateFindManyDraft(params);
    case "findDistinct":
      return validateFindDistinctDraft(params);
    case "count":
      return validateCountDraft(params);
    case "countRelations":
      return validateCountRelationsDraft(params);
    case "aggregate":
      return validateAggregateDraft(params);
    case "aggregateGroups":
      return validateAggregateGroupsDraft(params);
    case "groupBy":
      return validateGroupByDraft(params);
    default:
      return {
        status: "invalid",
        issues: [
          {
            path: "operation",
            message: `Unsupported operation: ${params.operation}`,
            code: "unsupported_operation",
          },
        ],
      };
  }
}

function validateFindManyDraft(
  params: ValidateOperationParametersDraftParams,
): OperationParametersValidationResult {
  const baseColumns = params.tableSchema.columns.map((column) => column.name);
  const baseComputedColumns = (params.tableSchema.computedColumns ?? []).map(
    (column) => column.name,
  );
  const { aliasToTable, joinOptions } = generateJoinGraph(
    params.schema,
    params.tableName,
    2,
  );
  const selectedJoinAliasTableSchemas = buildSelectedJoinAliasTableSchemas(
    params.schema,
    params.candidateOperationParameters,
    joinOptions,
  );
  const selectableTableColumns = Object.entries(aliasToTable).reduce<
    Record<string, string[]>
  >((acc, [alias, tableName]) => {
    const tableSchema = params.schema.find((table) => table.name === tableName);
    if (!tableSchema) {
      return acc;
    }
    acc[alias] = [
      ...tableSchema.columns.map((column) => column.name),
      ...(tableSchema.computedColumns ?? []).map((column) => column.name),
    ];
    return acc;
  }, {});

  for (const [alias, tableSchema] of Object.entries(
    selectedJoinAliasTableSchemas,
  )) {
    selectableTableColumns[alias] = collectSelectableColumns(tableSchema);
  }

  const context: FindManyValidatorContext = {
    baseTable: params.tableName,
    selectableTableColumns,
    baseColumns,
    baseComputedColumns,
    joinOptions,
  };

  return mapValidationResult(
    validateFindManyCandidate(params.candidateOperationParameters, context),
  );
}

function validateFindDistinctDraft(
  params: ValidateOperationParametersDraftParams,
): OperationParametersValidationResult {
  const selectableColumns = [
    ...params.tableSchema.columns.map(
      (column) => `${params.tableName}.${column.name}`,
    ),
    ...(params.tableSchema.computedColumns ?? []).map(
      (column) => `${params.tableName}.${column.name}`,
    ),
  ];

  const context: FindDistinctValidatorContext = {
    baseTable: params.tableName,
    selectableColumns,
  };

  return mapValidationResult(
    validateFindDistinctCandidate(params.candidateOperationParameters, context),
  );
}

function validateCountDraft(
  params: ValidateOperationParametersDraftParams,
): OperationParametersValidationResult {
  const joinGraph = generateJoinGraph(params.schema, params.tableName, 2);
  const selectedJoinAliasTableSchemas = buildSelectedJoinAliasTableSchemas(
    params.schema,
    params.candidateOperationParameters,
    joinGraph.joinOptions,
  );
  const context: CountValidatorContext = {
    baseTable: params.tableName,
    baseColumns: params.tableSchema.columns.map((column) => column.name),
    computedColumns: (params.tableSchema.computedColumns ?? []).map(
      (column) => column.name,
    ),
    joinOptions: joinGraph.joinOptions,
    joinAliasColumns: buildJoinAliasColumns(
      joinGraph.joinOptions,
      selectedJoinAliasTableSchemas,
    ),
  };

  return mapValidationResult(
    validateCountCandidate(params.candidateOperationParameters, context),
  );
}

function validateCountRelationsDraft(
  params: ValidateOperationParametersDraftParams,
): OperationParametersValidationResult {
  const relationOptions: CountRelationsValidatorContext["relationOptions"] = [];

  for (const relation of params.tableSchema.outwardRelations ?? []) {
    if (!relation.isList) {
      continue;
    }

    const target = params.schema.find(
      (table) => table.name === relation.targetTable.name,
    );
    if (!target) {
      continue;
    }

    const sourceColumns = relation.sourceColumns ?? [];
    const targetColumns = relation.targetColumns ?? [];
    if (sourceColumns.length === 0 || targetColumns.length === 0) {
      continue;
    }

    relationOptions.push({
      name: relation.name,
      table: relation.targetTable.name,
      path: [
        {
          source: sourceColumns.map((column) => `${params.tableName}.${column}`),
          target: targetColumns.map(
            (column) => `${relation.targetTable.name}.${column}`,
          ),
        },
      ],
      targetColumns: target.columns.map((column) => column.name),
    });
  }

  const context: CountRelationsValidatorContext = {
    baseColumns: params.tableSchema.columns.map((column) => column.name),
    relationOptions,
  };

  return mapValidationResult(
    validateCountRelationsCandidate(
      params.candidateOperationParameters,
      context,
    ),
  );
}

function validateAggregateDraft(
  params: ValidateOperationParametersDraftParams,
): OperationParametersValidationResult {
  const joinGraph = generateJoinGraph(params.schema, params.tableName, 2);
  const selectedJoinAliasTableSchemas = buildSelectedJoinAliasTableSchemas(
    params.schema,
    params.candidateOperationParameters,
    joinGraph.joinOptions,
  );
  const context: AggregateValidatorContext = {
    baseTable: params.tableName,
    joinOptions: joinGraph.joinOptions,
    columnCatalog: buildAggregateColumnCatalog(
      params.schema,
      params.tableName,
      params.tableSchema,
      joinGraph.joinOptions,
      selectedJoinAliasTableSchemas,
    ),
  };

  return mapValidationResult(
    validateAggregateCandidate(params.candidateOperationParameters, context),
  );
}

function validateAggregateGroupsDraft(
  params: ValidateOperationParametersDraftParams,
): OperationParametersValidationResult {
  const joinGraph = generateJoinGraph(params.schema, params.tableName, 2);
  const selectedJoinAliasTableSchemas = buildSelectedJoinAliasTableSchemas(
    params.schema,
    params.candidateOperationParameters,
    joinGraph.joinOptions,
  );
  const columnSets = buildQualifiedColumnSets(
    params.schema,
    params.tableName,
    params.tableSchema,
    joinGraph.joinOptions,
    selectedJoinAliasTableSchemas,
  );
  const context: AggregateGroupsValidatorContext = {
    baseTableName: params.tableName,
    joinOptions: joinGraph.joinOptions,
    allColumns: columnSets.allColumns,
    groupableColumns: columnSets.groupableColumns,
    intervalColumns: columnSets.intervalColumns,
    numericalColumns: columnSets.numericalColumns,
  };

  return mapValidationResult(
    validateAggregateGroupsCandidate(
      params.candidateOperationParameters,
      context,
    ),
  );
}

function validateGroupByDraft(
  params: ValidateOperationParametersDraftParams,
): OperationParametersValidationResult {
  const joinGraph = generateJoinGraph(params.schema, params.tableName, 2);
  const selectedJoinAliasTableSchemas = buildSelectedJoinAliasTableSchemas(
    params.schema,
    params.candidateOperationParameters,
    joinGraph.joinOptions,
  );
  const columnSets = buildQualifiedColumnSets(
    params.schema,
    params.tableName,
    params.tableSchema,
    joinGraph.joinOptions,
    selectedJoinAliasTableSchemas,
  );
  const context: GroupByValidatorContext = {
    baseTableName: params.tableName,
    joinOptions: joinGraph.joinOptions,
    allColumns: columnSets.allColumns,
    groupableColumns: columnSets.groupableColumns,
    intervalColumns: columnSets.intervalColumns,
    numericalColumns: columnSets.numericalColumns,
  };

  return mapValidationResult(
    validateGroupByCandidate(params.candidateOperationParameters, context),
  );
}

function buildAggregateColumnCatalog(
  schema: Schema,
  baseTableName: string,
  baseTableSchema: Schema[number],
  joinOptions: GeneratedJoinOption[],
  selectedJoinAliasTableSchemas: Record<string, Schema[number]>,
): AggregateColumnCatalog {
  const catalog: AggregateColumnCatalog = {};
  const temporalTypes = new Set(["DateTime", "Date"]);
  const schemaByName = new Map(
    schema.map((table) => [table.name, table] as const),
  );

  const register = (
    alias: string,
    columnName: string,
    metadata: AggregateColumnMetadata,
  ) => {
    catalog[alias] ??= {};
    const existing = catalog[alias][columnName];
    if (!existing) {
      catalog[alias][columnName] = metadata;
      return;
    }
    catalog[alias][columnName] = {
      isNumeric: existing.isNumeric || metadata.isNumeric,
      isTemporal: existing.isTemporal || metadata.isTemporal,
      isCountable: existing.isCountable || metadata.isCountable,
    };
  };

  for (const column of baseTableSchema.columns) {
    const columnType = column.effectiveType ?? column.type;
    register(baseTableName, column.name, {
      isNumeric:
        !isActiveEnumColumn(column.valueEnum) &&
        NUMERIC_LOGICAL_TYPES.has(columnType),
      isTemporal: temporalTypes.has(columnType),
      isCountable: true,
    });
  }

  for (const column of baseTableSchema.computedColumns ?? []) {
    register(baseTableName, column.name, {
      isNumeric: column.type === "number",
      isTemporal: false,
      isCountable: true,
    });
  }

  for (const option of joinOptions) {
    const tableSchema = schemaByName.get(option.table);
    if (!tableSchema) {
      continue;
    }

    for (const column of tableSchema.columns) {
      const columnType = column.effectiveType ?? column.type;
      register(option.name, column.name, {
        isNumeric:
          !isActiveEnumColumn(column.valueEnum) &&
          NUMERIC_LOGICAL_TYPES.has(columnType),
        isTemporal: temporalTypes.has(columnType),
        isCountable: true,
      });
    }

    for (const column of tableSchema.computedColumns ?? []) {
      register(option.name, column.name, {
        isNumeric: column.type === "number",
        isTemporal: false,
        isCountable: true,
      });
    }
  }

  for (const [alias, tableSchema] of Object.entries(
    selectedJoinAliasTableSchemas,
  )) {
    for (const column of tableSchema.columns) {
      const columnType = column.effectiveType ?? column.type;
      register(alias, column.name, {
        isNumeric:
          !isActiveEnumColumn(column.valueEnum) &&
          NUMERIC_LOGICAL_TYPES.has(columnType),
        isTemporal: temporalTypes.has(columnType),
        isCountable: true,
      });
    }

    for (const column of tableSchema.computedColumns ?? []) {
      register(alias, column.name, {
        isNumeric: column.type === "number",
        isTemporal: false,
        isCountable: true,
      });
    }
  }

  return catalog;
}

function buildQualifiedColumnSets(
  schema: Schema,
  baseTableName: string,
  baseTableSchema: Schema[number],
  joinOptions: GeneratedJoinOption[],
  selectedJoinAliasTableSchemas: Record<string, Schema[number]>,
) {
  const temporalTypes = new Set(["DateTime", "Date"]);
  const columnMetadata = new Map<
    string,
    { isTemporal: boolean; isNumeric: boolean }
  >();

  const registerTableColumns = (
    qualifier: string,
    tableSchema: Schema[number],
  ) => {
    for (const column of tableSchema.columns) {
      const columnType = column.effectiveType ?? column.type;
      const key = `${qualifier}.${column.name}`;
      columnMetadata.set(key, {
        isTemporal: temporalTypes.has(columnType),
        isNumeric:
          !isActiveEnumColumn(column.valueEnum) &&
          NUMERIC_LOGICAL_TYPES.has(columnType),
      });
    }

    for (const column of tableSchema.computedColumns ?? []) {
      columnMetadata.set(`${qualifier}.${column.name}`, {
        isTemporal: false,
        isNumeric: true,
      });
    }
  };

  registerTableColumns(baseTableName, baseTableSchema);

  for (const joinOption of joinOptions) {
    const tableSchema = schema.find((table) => table.name === joinOption.table);
    if (!tableSchema) {
      continue;
    }

    registerTableColumns(joinOption.name, tableSchema);

    if (joinOption.name !== joinOption.table) {
      registerTableColumns(joinOption.table, tableSchema);
    }
  }

  for (const [alias, tableSchema] of Object.entries(
    selectedJoinAliasTableSchemas,
  )) {
    registerTableColumns(alias, tableSchema);
  }

  const allColumns = Array.from(columnMetadata.keys());
  return {
    allColumns,
    intervalColumns: allColumns.filter(
      (column) => columnMetadata.get(column)?.isTemporal,
    ),
    numericalColumns: allColumns.filter(
      (column) => columnMetadata.get(column)?.isNumeric,
    ),
    groupableColumns: allColumns.filter(
      (column) => !columnMetadata.get(column)?.isTemporal,
    ),
  };
}

function buildSelectedJoinAliasTableSchemas(
  schema: Schema,
  candidateOperationParameters: Record<string, unknown>,
  joinOptions: GeneratedJoinOption[],
): Record<string, Schema[number]> {
  const joins = candidateOperationParameters.joins;
  if (!Array.isArray(joins) || joins.length === 0) {
    return {};
  }

  const joinOptionsByPathKey = new Map(
    joinOptions.map((option) => [joinPathKey(option.path), option] as const),
  );
  const selectedAliases: Record<string, Schema[number]> = {};

  for (const join of joins) {
    if (!join || typeof join !== "object") {
      continue;
    }

    const joinRecord = join as Record<string, unknown>;
    const alias =
      typeof joinRecord.name === "string" && joinRecord.name.trim().length > 0
        ? joinRecord.name.trim()
        : null;
    if (!alias) {
      continue;
    }

    const pathKey = joinPathKeyFromUnknown(joinRecord.path);
    const matchedOption = pathKey
      ? joinOptionsByPathKey.get(pathKey)
      : undefined;
    const tableName =
      matchedOption?.table ??
      (typeof joinRecord.table === "string" && joinRecord.table.trim().length > 0
        ? joinRecord.table.trim()
        : null);

    if (!tableName) {
      continue;
    }

    const tableSchema = schema.find((table) => table.name === tableName);
    if (!tableSchema) {
      continue;
    }

    selectedAliases[alias] = tableSchema;
  }

  return selectedAliases;
}

function buildJoinAliasColumns(
  joinOptions: GeneratedJoinOption[],
  selectedJoinAliasTableSchemas: Record<string, Schema[number]>,
): Record<string, string[]> {
  const aliasColumns: Record<string, string[]> = {};

  for (const option of joinOptions) {
    aliasColumns[option.name] = option.selectableColumns;
  }

  for (const [alias, tableSchema] of Object.entries(
    selectedJoinAliasTableSchemas,
  )) {
    aliasColumns[alias] = collectSelectableColumns(tableSchema);
  }

  return aliasColumns;
}

function collectSelectableColumns(tableSchema: Schema[number]) {
  return [
    ...tableSchema.columns.map((column) => column.name),
    ...(tableSchema.computedColumns ?? []).map((column) => column.name),
  ];
}

function joinPathKeyFromUnknown(path: unknown): string | null {
  if (!Array.isArray(path) || path.length === 0) {
    return null;
  }

  const normalizedPath: Array<{ source: string[]; target: string[] }> = [];
  for (const hop of path) {
    if (!hop || typeof hop !== "object") {
      return null;
    }

    const hopRecord = hop as Record<string, unknown>;
    if (
      !Array.isArray(hopRecord.source) ||
      !Array.isArray(hopRecord.target) ||
      !hopRecord.source.every((value) => typeof value === "string") ||
      !hopRecord.target.every((value) => typeof value === "string")
    ) {
      return null;
    }

    normalizedPath.push({
      source: hopRecord.source,
      target: hopRecord.target,
    });
  }

  return joinPathKey(normalizedPath);
}

function joinPathKey(path: Array<{ source: string[]; target: string[] }>) {
  return path
    .map((hop) => `${hop.source.join(",")}=>${hop.target.join(",")}`)
    .join("|");
}

function mapValidationResult(
  result:
    | FindManyValidationResult
    | FindDistinctValidationResult
    | CountValidationResult
    | CountRelationsValidationResult
    | AggregateValidationResult
    | AggregateGroupsValidationResult
    | GroupByValidationResult,
): OperationParametersValidationResult {
  if (result.status === "valid") {
    return {
      status: "valid",
      operationParameters: result.result as Record<string, unknown>,
    };
  }

  return {
    status: "invalid",
    issues: normalizeIssues(result.issues),
  };
}

function normalizeIssues(
  issues: Array<
    | FindManyInvalidResultIssue
    | FindDistinctInvalidIssue
    | CountInvalidIssue
    | CountRelationsInvalidResultIssue
    | AggregateInvalidIssue
    | AggregateGroupsInvalidResultIssue
    | GroupByInvalidResultIssue
  >,
): OperationParametersValidationIssue[] {
  return issues.map((issue) => ({
    path: issue.path,
    message: issue.message,
    code: issue.code,
  }));
}
