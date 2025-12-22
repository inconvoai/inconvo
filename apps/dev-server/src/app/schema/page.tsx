"use client";

import { useState, useEffect, useCallback, useMemo, Suspense } from "react";
import dynamic from "next/dynamic";
import { useRouter, usePathname, useSearchParams } from "next/navigation";
import {
  Box,
  Title,
  Text,
  Group,
  Button,
  Badge,
  Loader,
  Center,
  Alert,
} from "@mantine/core";
import {
  IconDatabaseEdit,
  IconRefresh,
  IconAlertCircle,
  IconCheck,
  IconInfoCircle,
} from "@tabler/icons-react";
import type {
  TableSummary,
  TableSchema,
  TableAccess,
  Column,
  ComputedColumn,
  Relation,
  RequestContextField,
  TableWithColumns,
  FilterValue,
} from "@repo/ui/semantic-model";

// Dynamic import to avoid Monaco SSR issues
const SemanticModelEditor = dynamic(
  () =>
    import("@repo/ui/semantic-model").then((mod) => mod.SemanticModelEditor),
  {
    ssr: false,
    loading: () => (
      <Center h="100%">
        <Loader size="lg" />
      </Center>
    ),
  },
);

const PER_PAGE = 20;

// API response types (from Prisma)
interface ApiTableId {
  id: string;
  name: string;
  access: string;
}

interface ApiColumn {
  id: string;
  name: string;
  rename: string | null;
  notes: string | null;
  type: string;
  selected: boolean;
  unit: string | null;
  conversion: {
    id: string;
    ast: unknown;
    type: string | null;
    selected: boolean;
  } | null;
}

interface ApiComputedColumn {
  id: string;
  name: string;
  ast: unknown;
  type: string;
  selected: boolean;
  unit: string | null;
  notes: string | null;
}

interface ApiRelation {
  id: string;
  name: string;
  targetTable: { id: string; name: string; access: string };
  targetTableId: string;
  isList: boolean;
  selected: boolean;
  source: string;
  status: string;
  errorTag: string | null;
  columnMappings: Array<{
    id: string;
    position: number;
    sourceColumnName: string;
    targetColumnName: string;
  }>;
}

interface ApiTableDetail {
  id: string;
  name: string;
  access: string;
  context: string | null;
  columns: ApiColumn[];
  computedColumns: ApiComputedColumn[];
  outwardRelations: ApiRelation[];
  condition: {
    column: { id: string; name: string };
    requestContextField: { id: string; key: string };
  } | null;
}

// Transform API data to shared component types
function transformColumn(api: ApiColumn): Column {
  return {
    id: api.id,
    name: api.name,
    rename: api.rename,
    notes: api.notes,
    type: api.type,
    effectiveType: api.conversion?.type ?? api.type,
    selected: api.selected,
    unit: api.unit,
    conversion: api.conversion
      ? {
          id: api.conversion.id,
          ast: api.conversion.ast,
          type: api.conversion.type,
          selected: api.conversion.selected,
        }
      : null,
    relation: [], // Not needed for display
  };
}

function transformComputedColumn(api: ApiComputedColumn): ComputedColumn {
  return {
    id: api.id,
    name: api.name,
    ast: api.ast,
    selected: api.selected,
    type: api.type,
    unit: api.unit,
    notes: api.notes,
  };
}

function transformRelation(api: ApiRelation): Relation {
  return {
    id: api.id,
    name: api.name,
    relationId: api.id,
    targetTable: {
      access: api.targetTable.access as TableAccess,
      name: api.targetTable.name,
    },
    targetTableId: api.targetTableId,
    isList: api.isList,
    selected: api.selected,
    source: api.source as "FK" | "MANUAL",
    status: (api.status as "VALID" | "BROKEN") ?? "VALID",
    errorTag: api.errorTag,
    columnMappings: api.columnMappings.map((m) => ({
      id: m.id,
      position: m.position,
      sourceColumnName: m.sourceColumnName,
      targetColumnName: m.targetColumnName,
    })),
  };
}

function transformTableSchema(api: ApiTableDetail): TableSchema {
  return {
    id: api.id,
    name: api.name,
    access: api.access as TableAccess,
    context: api.context,
    columns: api.columns.map(transformColumn),
    computedColumns: api.computedColumns.map(transformComputedColumn),
    relations: api.outwardRelations.map(transformRelation),
    condition: api.condition
      ? {
          column: {
            id: api.condition.column.id,
            name: api.condition.column.name,
          },
          requestContextField: {
            id: api.condition.requestContextField.id,
            key: api.condition.requestContextField.key,
          },
        }
      : null,
  };
}

// Convert URL access param to FilterValue
function accessParamToFilter(accessParam: string | null): FilterValue {
  if (!accessParam) return "active";
  const accessList = accessParam.split(",").sort().join(",");
  switch (accessList) {
    case "JOINABLE,OFF,QUERYABLE":
      return "all";
    case "QUERYABLE":
      return "queryable";
    case "JOINABLE":
      return "joinable";
    case "OFF":
      return "off";
    case "JOINABLE,QUERYABLE":
    default:
      return "active";
  }
}

// Convert FilterValue to URL access param
function filterToAccessParam(filter: FilterValue): string {
  switch (filter) {
    case "all":
      return "QUERYABLE,JOINABLE,OFF";
    case "queryable":
      return "QUERYABLE";
    case "joinable":
      return "JOINABLE";
    case "off":
      return "OFF";
    case "active":
    default:
      return "QUERYABLE,JOINABLE";
  }
}

function SchemaPageContent() {
  const router = useRouter();
  const pathname = usePathname();
  const searchParams = useSearchParams();

  // Read state from URL
  const searchFromUrl = searchParams.get("search") ?? "";
  const accessFromUrl = searchParams.get("access");
  const pageFromUrl = searchParams.get("page");

  const searchQuery = searchFromUrl;
  const accessFilter = accessParamToFilter(accessFromUrl);
  const currentPage = pageFromUrl ? parseInt(pageFromUrl, 10) : 1;

  const [tables, setTables] = useState<TableSummary[]>([]);
  const [totalCount, setTotalCount] = useState(0);
  const [selectedTableId, setSelectedTableId] = useState<string | null>(null);
  const [selectedTable, setSelectedTable] = useState<TableSchema | null>(null);
  const [requestContextFields, setRequestContextFields] = useState<
    RequestContextField[]
  >([]);
  const [loading, setLoading] = useState(true);
  const [detailLoading, setDetailLoading] = useState(false);
  const [syncing, setSyncing] = useState(false);
  const [error, setError] = useState<string | null>(null);
  const [successMessage, setSuccessMessage] = useState<string | null>(null);

  // Update URL query params
  const updateQueryParams = useCallback(
    (params: Partial<{ page: string; access: string; search: string }>) => {
      const newParams = new URLSearchParams(searchParams.toString());

      Object.entries(params).forEach(([key, value]) => {
        if (value === undefined || value === "") {
          newParams.delete(key);
        } else {
          newParams.set(key, value);
        }
      });

      const queryString = newParams.toString();
      router.push(queryString ? `${pathname}?${queryString}` : pathname);
    },
    [router, pathname, searchParams],
  );

  // Fetch tables list with current filters
  const fetchTables = useCallback(async (): Promise<TableSummary[]> => {
    try {
      const params = new URLSearchParams();
      params.set("access", filterToAccessParam(accessFilter));
      params.set("page", currentPage.toString());
      params.set("perPage", PER_PAGE.toString());
      if (searchQuery) {
        params.set("search", searchQuery);
      }

      const res = await fetch(`/api/schema/tables?${params.toString()}`);
      const data = (await res.json()) as {
        error?: string;
        tables?: ApiTableId[];
        totalCount?: number;
      };
      if (data.error) throw new Error(data.error);

      // Transform to TableSummary (minimal data)
      const transformed: TableSummary[] = (data.tables ?? []).map((table) => ({
        id: table.id,
        name: table.name,
        access: table.access as TableAccess,
        columnCount: 0,
        selectedColumnCount: 0,
        computedColumnCount: 0,
        relationCount: 0,
        hasCondition: false,
      }));

      setTables(transformed);
      setTotalCount(data.totalCount ?? 0);
      return transformed;
    } catch (err) {
      setError(err instanceof Error ? err.message : "Failed to fetch tables");
      return [];
    } finally {
      setLoading(false);
    }
  }, [accessFilter, currentPage, searchQuery]);

  // Fetch request context fields
  const fetchRequestContext = useCallback(async () => {
    try {
      const res = await fetch("/api/schema/request-context");
      const data = (await res.json()) as {
        error?: string;
        fields?: Array<{ id: string; key: string; type: string }>;
      };
      if (data.error) throw new Error(data.error);
      setRequestContextFields(
        (data.fields ?? []).map((f) => ({
          id: f.id,
          key: f.key,
          type: f.type as "STRING" | "NUMBER",
        })),
      );
    } catch (err) {
      console.error("Failed to fetch request context:", err);
    }
  }, []);

  // Fetch single table details
  const fetchTableDetail = useCallback(async (tableName: string) => {
    setDetailLoading(true);
    try {
      const res = await fetch(
        `/api/schema/tables/${encodeURIComponent(tableName)}`,
      );
      const data = (await res.json()) as {
        error?: string;
        table?: ApiTableDetail;
      };
      if (data.error) throw new Error(data.error);
      if (data.table) {
        setSelectedTable(transformTableSchema(data.table));
      }
    } catch (err) {
      setError(err instanceof Error ? err.message : "Failed to fetch table");
    } finally {
      setDetailLoading(false);
    }
  }, []);

  // Initial sync (called on first load if no tables)
  const doInitialSync = useCallback(async () => {
    setSyncing(true);
    try {
      const res = await fetch("/api/schema", { method: "POST" });
      const data = (await res.json()) as { error?: string };
      if (data.error) throw new Error(data.error);
    } catch (err) {
      setError(err instanceof Error ? err.message : "Failed to sync schema");
    } finally {
      setSyncing(false);
    }
  }, []);

  // Track if initial sync has been attempted
  const [initialSyncDone, setInitialSyncDone] = useState(false);

  // Fetch request context once on mount
  useEffect(() => {
    void fetchRequestContext();
  }, [fetchRequestContext]);

  // Fetch tables when URL params change
  useEffect(() => {
    void fetchTables();
  }, [fetchTables]);

  // Do initial sync if no tables on first load
  useEffect(() => {
    if (
      !loading &&
      !initialSyncDone &&
      tables.length === 0 &&
      totalCount === 0
    ) {
      setInitialSyncDone(true);
      void doInitialSync().then(() => fetchTables());
    }
  }, [
    loading,
    initialSyncDone,
    tables.length,
    totalCount,
    doInitialSync,
    fetchTables,
  ]);

  // Fetch table detail when selection changes
  useEffect(() => {
    if (selectedTableId) {
      const table = tables.find((t) => t.id === selectedTableId);
      if (table) {
        void fetchTableDetail(table.name);
      }
    } else {
      setSelectedTable(null);
    }
  }, [selectedTableId, tables, fetchTableDetail]);

  // Handle sync button
  const handleSync = async () => {
    setSyncing(true);
    setError(null);
    try {
      const res = await fetch("/api/schema", { method: "POST" });
      const data = (await res.json()) as {
        error?: string;
        added?: number;
        updated?: number;
      };
      if (data.error) throw new Error(data.error);
      setSuccessMessage(`Synced: ${data.added} added, ${data.updated} updated`);
      await fetchTables();
      setTimeout(() => setSuccessMessage(null), 3000);
    } catch (err) {
      setError(err instanceof Error ? err.message : "Failed to sync schema");
    } finally {
      setSyncing(false);
    }
  };

  // Callbacks for SemanticModelEditor
  const onTableSelect = useCallback((tableId: string) => {
    setSelectedTableId(tableId);
  }, []);

  const onUpdateTable = useCallback(
    async (
      tableId: string,
      payload: { access?: TableAccess; context?: string | null },
    ) => {
      const table = tables.find((t) => t.id === tableId);
      if (!table) return;

      const res = await fetch(
        `/api/schema/tables/${encodeURIComponent(table.name)}`,
        {
          method: "PATCH",
          headers: { "Content-Type": "application/json" },
          body: JSON.stringify(payload),
        },
      );
      const data = (await res.json()) as { error?: string };
      if (data.error) throw new Error(data.error);

      // Refresh data
      await fetchTables();
      if (selectedTableId === tableId) {
        await fetchTableDetail(table.name);
      }
    },
    [tables, selectedTableId, fetchTables, fetchTableDetail],
  );

  const onUpdateColumn = useCallback(
    async (
      tableId: string,
      columnId: string,
      payload: {
        selected?: boolean;
        rename?: string | null;
        notes?: string | null;
      },
    ) => {
      const res = await fetch(`/api/schema/columns/${columnId}`, {
        method: "PATCH",
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify(payload),
      });
      const data = (await res.json()) as { error?: string };
      if (data.error) throw new Error(data.error);

      // Update local state optimistically
      if (selectedTable?.id === tableId) {
        setSelectedTable({
          ...selectedTable,
          columns: selectedTable.columns.map((col) =>
            col.id === columnId ? { ...col, ...payload } : col,
          ),
        });
      }
    },
    [selectedTable],
  );

  const onUpdateRelation = useCallback(
    async (tableId: string, relationId: string, selected: boolean) => {
      const res = await fetch(`/api/schema/relations/${relationId}`, {
        method: "PATCH",
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify({ selected }),
      });
      const data = (await res.json()) as { error?: string };
      if (data.error) throw new Error(data.error);

      // Update local state optimistically
      if (selectedTable?.id === tableId) {
        setSelectedTable({
          ...selectedTable,
          relations: selectedTable.relations.map((rel) =>
            rel.id === relationId ? { ...rel, selected } : rel,
          ),
        });
      }
    },
    [selectedTable],
  );

  // Handle search change (TableList handles debouncing, we just update URL)
  const handleSearchChange = useCallback(
    (query: string) => {
      updateQueryParams({ search: query, page: "1" });
    },
    [updateQueryParams],
  );

  // Handle access filter change
  const handleAccessFilterChange = useCallback(
    (filter: FilterValue) => {
      updateQueryParams({
        access: filterToAccessParam(filter),
        page: "1",
      });
    },
    [updateQueryParams],
  );

  // Handle page change
  const handlePageChange = useCallback(
    (page: number) => {
      updateQueryParams({ page: page.toString() });
    },
    [updateQueryParams],
  );

  // Available tables for relations (empty for now - manual relations not supported)
  const availableTables: TableWithColumns[] = useMemo(() => [], []);

  if (loading) {
    return (
      <Center h="100%">
        <Loader size="lg" />
      </Center>
    );
  }

  return (
    <Box h="100%" style={{ display: "flex", flexDirection: "column" }}>
      {/* Header */}
      <Box
        px="md"
        py="sm"
        style={{ borderBottom: "1px solid var(--mantine-color-gray-3)" }}
      >
        <Group justify="space-between" mb="xs">
          <Group gap="sm">
            <IconDatabaseEdit size={24} />
            <Title order={3}>Semantic Model</Title>
            {totalCount > 0 && (
              <Badge variant="light" size="md">
                {totalCount} tables
              </Badge>
            )}
          </Group>
          <Button
            leftSection={<IconRefresh size={16} />}
            onClick={handleSync}
            loading={syncing}
            variant="light"
            size="sm"
          >
            Sync from Database
          </Button>
        </Group>

        {/* Info about limited features */}
        <Alert
          icon={<IconInfoCircle size={14} />}
          color="blue"
          variant="light"
          py="xs"
        >
          <Text size="xs">
            <strong>Dev Server Mode:</strong> Schema augmentation features
            (computed columns, manual relations, column conversions) are
            available in the full platform.
          </Text>
        </Alert>

        {error && (
          <Alert
            icon={<IconAlertCircle size={14} />}
            color="red"
            onClose={() => setError(null)}
            withCloseButton
            mt="xs"
            py="xs"
          >
            {error}
          </Alert>
        )}

        {successMessage && (
          <Alert
            icon={<IconCheck size={14} />}
            color="green"
            onClose={() => setSuccessMessage(null)}
            withCloseButton
            mt="xs"
            py="xs"
          >
            {successMessage}
          </Alert>
        )}
      </Box>

      {/* Main Editor - takes remaining space */}
      <Box style={{ flex: 1, minHeight: 0 }}>
        <SemanticModelEditor
          tables={tables}
          selectedTable={selectedTable}
          requestContextFields={requestContextFields}
          availableTables={availableTables}
          listLoading={syncing}
          detailLoading={detailLoading}
          onTableSelect={onTableSelect}
          // Server-side filtering props (URL-based)
          searchQuery={searchQuery}
          onSearchChange={handleSearchChange}
          accessFilter={accessFilter}
          onAccessFilterChange={handleAccessFilterChange}
          totalTableCount={totalCount}
          currentPage={currentPage}
          onPageChange={handlePageChange}
          perPage={PER_PAGE}
          // Mutation callbacks
          onUpdateTable={onUpdateTable}
          onUpdateColumn={onUpdateColumn}
          onUpdateRelation={onUpdateRelation}
          // Schema augmentation features not available in dev server
        />
      </Box>
    </Box>
  );
}

// Wrap in Suspense for useSearchParams
export default function SchemaPage() {
  return (
    <Suspense
      fallback={
        <Center h="100%">
          <Loader size="lg" />
        </Center>
      }
    >
      <SchemaPageContent />
    </Suspense>
  );
}
