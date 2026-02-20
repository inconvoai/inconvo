import {
  NUMERIC_LOGICAL_TYPES,
  normalizeColumnValueEnum,
  type Query,
  type QueryResponse,
  type Schema,
  type SchemaColumn,
  type SchemaColumnConversion,
  resolveEffectiveColumnType,
} from "@repo/types";
import throat from "throat";

export const MAX_ENUM_ENTRIES = 50;
export const ENUM_FETCH_LIMIT = MAX_ENUM_ENTRIES + 1;
export const DYNAMIC_ENUM_CACHE_CLEANUP_THRESHOLD = 200;
export const DYNAMIC_ENUM_CACHE_MAX_ENTRIES = 1000;
export const NUMERIC_OR_STRING_LOGICAL_TYPES = new Set<string>([
  "string",
  ...NUMERIC_LOGICAL_TYPES,
]);

export type ValueEnumEntryInput = {
  value: string | number;
  label: string;
  selected?: boolean;
  position?: number;
};

export type NormalizedValueEnumEntry = {
  value: string | number;
  label: string;
  selected: boolean;
  position: number;
};

export type UserContext = Record<string, string | number | boolean> | null | undefined;

export function getTrueBooleanKeys(userContext: UserContext): string[] {
  if (!userContext) return [];
  return Object.entries(userContext)
    .filter(([, value]) => value === true)
    .map(([key]) => key);
}

export function accessPolicyWhereClause(userContext: UserContext): {
  OR: Array<Record<string, unknown>>;
} {
  const trueKeys = getTrueBooleanKeys(userContext);

  return {
    OR: [
      { accessPolicy: null },
      ...(trueKeys.length > 0
        ? [
            {
              accessPolicy: {
                userContextField: { key: { in: trueKeys } },
              },
            },
          ]
        : []),
    ],
  };
}

export function pruneRelationsToExcludedTables(schema: Schema): Schema {
  const allowedTableNames = new Set(schema.map((table) => table.name));

  return schema.map((table) => ({
    ...table,
    columns: table.columns.map((column) => ({
      ...column,
      relation: column.relation.filter((entry) => {
        const targetTableName = entry.relation?.targetTable?.name;
        return targetTableName ? allowedTableNames.has(targetTableName) : true;
      }),
    })),
    outwardRelations: table.outwardRelations.filter((relation) =>
      allowedTableNames.has(relation.targetTable.name),
    ),
  }));
}

export type ColumnRelationMappingInput = {
  relation?: {
    targetTable?: { name: string | null } | null;
  } | null;
  targetColumn?: { name: string | null } | null;
  targetColumnName: string | null;
};

export function mapColumnRelationEntries(
  mappings: ColumnRelationMappingInput[],
): SchemaColumn["relation"] {
  return mappings.map((mapping) => ({
    relation: {
      targetTable: {
        name: mapping.relation?.targetTable?.name ?? null,
      },
    },
    targetColumn: {
      name: mapping.targetColumn?.name ?? mapping.targetColumnName ?? null,
    },
  }));
}

export type RelationColumnMappingInput = {
  position: number;
  sourceColumn?: { name: string | null } | null;
  targetColumn?: { name: string | null } | null;
  sourceColumnName: string | null;
  targetColumnName: string | null;
};

export type ColumnRenameConflict = {
  type: "column" | "computedColumn";
  name: string;
};

type ColumnRenameConflictColumnInput = {
  id: string;
  name: string;
  rename?: string | null;
};

type ColumnRenameConflictComputedColumnInput = {
  name: string;
  selected?: boolean | null;
};

export function extractOrderedRelationColumns(
  mappings: RelationColumnMappingInput[],
): { sourceColumns: string[]; targetColumns: string[] } {
  const sortedMappings = [...mappings].sort((a, b) => a.position - b.position);
  const sourceColumns = sortedMappings
    .map((mapping) => mapping.sourceColumn?.name ?? mapping.sourceColumnName)
    .filter((name): name is string =>
      Boolean(name && name.trim().length > 0),
    );
  const targetColumns = sortedMappings
    .map((mapping) => mapping.targetColumn?.name ?? mapping.targetColumnName)
    .filter((name): name is string =>
      Boolean(name && name.trim().length > 0),
    );
  return { sourceColumns, targetColumns };
}

/**
 * Finds whether a column rename would collide with an existing column name/effective
 * name or computed column name in the same table.
 */
export function findColumnRenameConflict(params: {
  currentColumnId: string;
  nextEffectiveName: string;
  tableColumns: ColumnRenameConflictColumnInput[];
  computedColumns?: ColumnRenameConflictComputedColumnInput[] | null;
}): ColumnRenameConflict | null {
  const normalizedNextName = params.nextEffectiveName.trim();
  if (!normalizedNextName) {
    return null;
  }

  const columnConflict = params.tableColumns.find((column) => {
    if (column.id === params.currentColumnId) {
      return false;
    }
    const effectiveName = (column.rename ?? column.name).trim();
    return effectiveName === normalizedNextName || column.name === normalizedNextName;
  });
  if (columnConflict) {
    return {
      type: "column",
      name: columnConflict.name,
    };
  }

  const computedColumnConflict = (params.computedColumns ?? []).find(
    (computedColumn) =>
      computedColumn.selected !== false &&
      computedColumn.name.trim() === normalizedNextName,
  );
  if (computedColumnConflict) {
    return {
      type: "computedColumn",
      name: computedColumnConflict.name,
    };
  }

  return null;
}

export function mapSchemaCondition(params: {
  includeCondition: boolean;
  condition:
    | {
        column: { name: string };
        userContextField?: { key: string } | null;
      }
    | null
    | undefined;
}): Schema[number]["condition"] {
  const { includeCondition, condition } = params;
  if (!includeCondition || !condition?.userContextField) {
    return null;
  }
  return {
    column: { name: condition.column.name },
    userContextField: { key: condition.userContextField.key },
  };
}

export function mapSchemaAccessPolicy(accessPolicy: {
  userContextField?: { key: string } | null;
} | null | undefined): Schema[number]["accessPolicy"] {
  if (!accessPolicy?.userContextField) {
    return null;
  }
  return {
    userContextField: {
      key: accessPolicy.userContextField.key,
    },
  };
}

export type ColumnAugmentationInput = {
  id: string;
  kind: "CONVERSION" | "STATIC_ENUM" | "DYNAMIC_ENUM";
  selected: boolean;
  conversionConfig: {
    ast: unknown;
    type: string | null;
  } | null;
  staticEnumConfig: {
    entries: unknown;
  } | null;
  dynamicEnumConfig: {
    id: string;
  } | null;
};

export function buildColumnProjectionFromAugmentation(params: {
  columnType: string;
  augmentation: ColumnAugmentationInput | null | undefined;
  parseConversionAst?: (ast: unknown) => unknown;
}): {
  conversion: SchemaColumnConversion | null;
  enumMode: "STATIC" | "DYNAMIC" | null;
  valueEnum: SchemaColumn["valueEnum"];
  effectiveType: string;
} {
  const { columnType, augmentation, parseConversionAst } = params;
  const parseAst = parseConversionAst ?? ((ast: unknown) => ast);

  const conversion: SchemaColumnConversion | null =
    augmentation?.kind === "CONVERSION" && augmentation.conversionConfig
      ? {
          id: augmentation.id,
          ast: parseAst(augmentation.conversionConfig.ast),
          type: augmentation.conversionConfig.type ?? null,
          selected: augmentation.selected,
        }
      : null;

  const enumMode: "STATIC" | "DYNAMIC" | null =
    augmentation?.kind === "STATIC_ENUM"
      ? "STATIC"
      : augmentation?.kind === "DYNAMIC_ENUM"
        ? "DYNAMIC"
        : null;

  const valueEnumInput =
    augmentation?.kind === "STATIC_ENUM" && augmentation.staticEnumConfig
      ? {
          id: augmentation.id,
          selected: augmentation.selected,
          entries: augmentation.staticEnumConfig.entries,
        }
      : augmentation?.kind === "DYNAMIC_ENUM" && augmentation.dynamicEnumConfig
        ? {
            id: augmentation.id,
            selected: augmentation.selected,
            entries: [],
          }
        : null;

  return {
    conversion,
    enumMode,
    valueEnum: normalizeColumnValueEnum(valueEnumInput),
    effectiveType: resolveEffectiveColumnType(columnType, conversion),
  };
}

export function normalizeStaticEnumEntries(params: {
  entries: ValueEnumEntryInput[];
  effectiveType: string;
}): NormalizedValueEnumEntry[] {
  const { entries, effectiveType } = params;
  const isNumeric = NUMERIC_LOGICAL_TYPES.has(effectiveType);

  if (!entries.length) {
    throw new Error("Enum must include at least one entry.");
  }

  const normalized = entries.map((entry, index) => ({
    value: entry.value,
    label: entry.label?.trim(),
    selected: entry.selected ?? true,
    position: entry.position ?? index,
  }));

  const valueKeys = new Set<string>();
  const labelKeys = new Set<string>();

  return normalized
    .map((entry) => {
      if (!entry.label || entry.label.length === 0) {
        throw new Error("Enum labels must not be empty.");
      }

      let canonicalValue: string | number;
      if (isNumeric) {
        if (typeof entry.value === "number" && Number.isFinite(entry.value)) {
          canonicalValue = entry.value;
        } else if (
          typeof entry.value === "string" &&
          Number.isFinite(Number(entry.value))
        ) {
          canonicalValue = Number(entry.value);
        } else {
          throw new Error("Enum values must be numeric for numeric columns.");
        }
      } else {
        if (
          typeof entry.value !== "string" ||
          entry.value.trim().length === 0
        ) {
          throw new Error(
            "Enum values must be non-empty strings for string columns.",
          );
        }
        canonicalValue = entry.value.trim();
      }

      const valueKey =
        typeof canonicalValue === "number"
          ? `n:${canonicalValue}`
          : `s:${canonicalValue.toLowerCase()}`;
      if (valueKeys.has(valueKey)) {
        throw new Error(`Duplicate enum value "${String(canonicalValue)}".`);
      }
      valueKeys.add(valueKey);

      const labelKey = `l:${entry.label.toLowerCase()}`;
      if (labelKeys.has(labelKey)) {
        throw new Error(`Duplicate enum label "${entry.label}".`);
      }
      labelKeys.add(labelKey);

      return {
        value: canonicalValue,
        label: entry.label,
        selected: entry.selected,
        position: entry.position,
      };
    })
    .sort((a, b) => a.position - b.position);
}

export function normalizeDistinctEntriesForEnum(params: {
  rows: unknown;
  tableName: string;
  columnName: string;
  effectiveType: string;
  fetchLimit?: number;
}): NormalizedValueEnumEntry[] {
  const {
    rows,
    tableName,
    columnName,
    effectiveType,
    fetchLimit = ENUM_FETCH_LIMIT,
  } = params;
  if (!Array.isArray(rows)) {
    return [];
  }

  const isNumeric = NUMERIC_LOGICAL_TYPES.has(effectiveType);
  const qualifiedColumn = `${tableName}.${columnName}`;
  const seen = new Set<string>();
  const entries: NormalizedValueEnumEntry[] = [];

  for (const row of rows) {
    let rawValue: unknown = row;
    if (row && typeof row === "object" && !Array.isArray(row)) {
      const record = row as Record<string, unknown>;
      rawValue =
        record[qualifiedColumn] ??
        record[columnName] ??
        Object.values(record)[0];
    }

    let canonicalValue: string | number | null = null;
    if (isNumeric) {
      if (typeof rawValue === "number" && Number.isFinite(rawValue)) {
        canonicalValue = rawValue;
      } else if (
        typeof rawValue === "string" &&
        rawValue.trim().length > 0 &&
        Number.isFinite(Number(rawValue))
      ) {
        canonicalValue = Number(rawValue);
      }
    } else if (typeof rawValue === "string") {
      const trimmed = rawValue.trim();
      if (trimmed.length > 0) {
        canonicalValue = trimmed;
      }
    } else if (typeof rawValue === "number" && Number.isFinite(rawValue)) {
      canonicalValue = String(rawValue);
    }

    if (canonicalValue === null) {
      continue;
    }

    const dedupeKey =
      typeof canonicalValue === "number"
        ? `n:${canonicalValue}`
        : `s:${canonicalValue.toLowerCase()}`;
    if (seen.has(dedupeKey)) {
      continue;
    }
    seen.add(dedupeKey);

    entries.push({
      value: canonicalValue,
      label: String(canonicalValue),
      selected: true,
      position: entries.length,
    });

    if (entries.length >= fetchLimit) {
      break;
    }
  }

  return entries;
}

export function buildDynamicEnumWhereAndArray(params: {
  table: Schema[number];
  userContext: Record<string, string | number | boolean>;
}): Array<Record<string, { equals: string | number | boolean }>> {
  const { table, userContext } = params;
  const condition = table.condition;
  if (!condition) {
    return [];
  }

  const value = userContext[condition.userContextField.key];
  if (value === undefined || value === null) {
    return [];
  }

  const matchingColumn = table.columns.find(
    (column) => column.name === condition.column.name,
  );
  const conditionColumn = matchingColumn?.dbName ?? condition.column.name;
  const qualifiedColumn = `${table.name}.${conditionColumn}`;
  return [{ [qualifiedColumn]: { equals: value } }];
}

export function isFindDistinctLimitHitError(
  error: unknown,
  fetchLimit: number,
): boolean {
  return (
    error instanceof Error &&
    error.message.includes(`Find Distinct limit hit at ${fetchLimit}`)
  );
}

export type DynamicEnumCacheEntry = {
  timestamp: number;
  entries: NormalizedValueEnumEntry[] | null;
};
export type DynamicEnumCache = Map<string, DynamicEnumCacheEntry>;

function getFreshDynamicEnumCacheEntry(params: {
  cache: DynamicEnumCache;
  cacheKey: string;
  cacheTtlMs: number;
}): DynamicEnumCacheEntry | null {
  const { cache, cacheKey, cacheTtlMs } = params;
  const entry = cache.get(cacheKey);
  if (!entry) {
    return null;
  }

  if (Date.now() - entry.timestamp >= cacheTtlMs) {
    cache.delete(cacheKey);
    return null;
  }

  // Touch key on read so Map insertion order acts like LRU recency.
  cache.delete(cacheKey);
  cache.set(cacheKey, entry);
  return entry;
}

function enforceDynamicEnumCacheSize(params: {
  cache: DynamicEnumCache;
  maxEntries: number;
}): void {
  const { cache } = params;
  const maxEntries = Math.max(1, params.maxEntries);
  while (cache.size > maxEntries) {
    const oldestKey = cache.keys().next().value;
    if (!oldestKey) {
      break;
    }
    cache.delete(oldestKey);
  }
}

function setDynamicEnumCacheEntry(params: {
  cache: DynamicEnumCache;
  cacheKey: string;
  entry: DynamicEnumCacheEntry;
  maxEntries: number;
}): void {
  const { cache, cacheKey, entry, maxEntries } = params;
  if (cache.has(cacheKey)) {
    cache.delete(cacheKey);
  }
  cache.set(cacheKey, entry);
  enforceDynamicEnumCacheSize({ cache, maxEntries });
}

function buildDynamicEnumCacheKey(params: {
  connectionId: string;
  tableSchema: string | null;
  tableName: string;
  columnDbName: string;
  fetchLimit: number;
  whereAndArray: Array<Record<string, { equals: string | number | boolean }>>;
}): string {
  return `dyn:${params.connectionId}:${params.tableSchema ?? ""}:${params.tableName}:${params.columnDbName}:${params.fetchLimit}:${JSON.stringify(params.whereAndArray)}`;
}

export type DynamicEnumConnector = {
  query(query: Query): Promise<QueryResponse>;
};

export async function resolveDynamicEnumsForSchema(params: {
  schema: Schema;
  connector: DynamicEnumConnector;
  userContext: Record<string, string | number | boolean>;
  connectionId: string;
  cache: DynamicEnumCache;
  fetchLimit?: number;
  concurrencyLimit?: number;
  cacheTtlMs?: number;
  cacheMaxEntries?: number;
  onError?: (params: {
    tableName: string;
    columnName: string;
    error: unknown;
  }) => void;
}): Promise<Schema> {
  const {
    schema,
    connector,
    userContext,
    connectionId,
    cache,
    fetchLimit = ENUM_FETCH_LIMIT,
    concurrencyLimit = 5,
    cacheTtlMs = 60_000,
    cacheMaxEntries = DYNAMIC_ENUM_CACHE_MAX_ENTRIES,
    onError,
  } = params;
  const normalizedCacheMaxEntries = Math.max(1, cacheMaxEntries);

  const hasDynamicEnums = schema.some((table) =>
    table.columns.some(
      (column) =>
        column.enumMode === "DYNAMIC" && column.valueEnum?.selected !== false,
    ),
  );
  if (!hasDynamicEnums) {
    return schema;
  }

  const limit = throat(concurrencyLimit);

  function fetchOrCacheDistinctEntries(fetchParams: {
    table: Schema[number];
    column: SchemaColumn;
    effectiveType: string;
    whereAndArray: Array<Record<string, { equals: string | number | boolean }>>;
  }): Promise<NormalizedValueEnumEntry[] | null> {
    const { table, column, effectiveType, whereAndArray } = fetchParams;
    const cacheKey = buildDynamicEnumCacheKey({
      connectionId,
      tableSchema: table.schema ?? null,
      tableName: table.name,
      columnDbName: column.dbName,
      fetchLimit,
      whereAndArray,
    });

    const cached = getFreshDynamicEnumCacheEntry({
      cache,
      cacheKey,
      cacheTtlMs,
    });
    if (cached) {
      return Promise.resolve(cached.entries);
    }

    return limit(async () => {
      // Re-check cache after acquiring the semaphore slot
      const rechecked = getFreshDynamicEnumCacheEntry({
        cache,
        cacheKey,
        cacheTtlMs,
      });
      if (rechecked) {
        return rechecked.entries;
      }

      let queryResponse: Awaited<ReturnType<typeof connector.query>>;
      try {
        queryResponse = await connector.query({
          table: table.name,
          tableSchema: table.schema ?? null,
          whereAndArray,
          tableConditions: null,
          operation: "findDistinct",
          operationParameters: {
            column: `${table.name}.${column.dbName}`,
            limit: fetchLimit,
          },
        });
      } catch (error) {
        if (isFindDistinctLimitHitError(error, fetchLimit)) {
          setDynamicEnumCacheEntry({
            cache,
            cacheKey,
            entry: { timestamp: Date.now(), entries: null },
            maxEntries: normalizedCacheMaxEntries,
          });
          return null;
        }
        onError?.({
          tableName: table.name,
          columnName: column.dbName,
          error,
        });
        setDynamicEnumCacheEntry({
          cache,
          cacheKey,
          entry: { timestamp: Date.now(), entries: null },
          maxEntries: normalizedCacheMaxEntries,
        });
        return null;
      }

      const entries = normalizeDistinctEntriesForEnum({
        rows: queryResponse.data,
        tableName: table.name,
        columnName: column.dbName,
        effectiveType,
        fetchLimit,
      });
      if (entries.length === 0 || entries.length >= fetchLimit) {
        setDynamicEnumCacheEntry({
          cache,
          cacheKey,
          entry: { timestamp: Date.now(), entries: null },
          maxEntries: normalizedCacheMaxEntries,
        });
        return null;
      }

      setDynamicEnumCacheEntry({
        cache,
        cacheKey,
        entry: { timestamp: Date.now(), entries },
        maxEntries: normalizedCacheMaxEntries,
      });
      return entries;
    });
  }

  // Lazy cache cleanup: prune expired entries when cache grows large
  if (cache.size > DYNAMIC_ENUM_CACHE_CLEANUP_THRESHOLD) {
    const now = Date.now();
    for (const [key, entry] of cache) {
      if (now - entry.timestamp >= cacheTtlMs) {
        cache.delete(key);
      }
    }
    enforceDynamicEnumCacheSize({
      cache,
      maxEntries: normalizedCacheMaxEntries,
    });
  }

  return await Promise.all(
    schema.map(async (table) => {
      const hasDynamicColumns = table.columns.some(
        (column) =>
          column.enumMode === "DYNAMIC" && column.valueEnum?.selected !== false,
      );
      if (!hasDynamicColumns) {
        return table;
      }

      const whereAndArray = buildDynamicEnumWhereAndArray({
        table,
        userContext,
      });

      const columns = await Promise.all(
        table.columns.map(async (column) => {
          if (column.enumMode !== "DYNAMIC" || column.valueEnum?.selected === false) {
            return column;
          }

          const effectiveType = (column.effectiveType ?? column.type).toLowerCase();
          if (
            effectiveType !== "string" &&
            !NUMERIC_LOGICAL_TYPES.has(effectiveType)
          ) {
            return { ...column, valueEnum: null };
          }

          const entries = await fetchOrCacheDistinctEntries({
            table,
            column,
            effectiveType,
            whereAndArray,
          });

          if (!entries) {
            return { ...column, valueEnum: null };
          }

          return {
            ...column,
            valueEnum: {
              id:
                column.valueEnum?.id ??
                `dyn_${table.name.replace(/\./g, "_")}_${column.dbName.replace(/\./g, "_")}`,
              selected: column.valueEnum?.selected ?? true,
              entries,
            },
          };
        }),
      );

      return {
        ...table,
        columns,
      };
    }),
  );
}
