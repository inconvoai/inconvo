"use client";

import { useState, useEffect, useCallback, Suspense } from "react";
import dynamic from "next/dynamic";
import { useRouter, usePathname, useSearchParams } from "next/navigation";
import {
  Box,
  Title,
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
} from "@tabler/icons-react";
import type {
  TableSummary,
  TableSchema,
  TableAccess,
  Column,
  ComputedColumn,
  Relation,
  UserContextField,
  TableWithColumns,
  FilterValue,
} from "@repo/ui/semantic-model";
import posthog from "posthog-js";
import { trackFeatureUsageClient } from "~/lib/telemetry";

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
    userContextField: { id: string; key: string };
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
          userContextField: {
            id: api.condition.userContextField.id,
            key: api.condition.userContextField.key,
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
  const [userContextFields, setUserContextFields] = useState<
    UserContextField[]
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

  // Fetch user context fields
  const fetchUserContext = useCallback(async () => {
    try {
      const res = await fetch("/api/schema/user-context");
      const data = (await res.json()) as {
        error?: string;
        fields?: Array<{ id: string; key: string; type: string }>;
      };
      if (data.error) throw new Error(data.error);
      setUserContextFields(
        (data.fields ?? []).map((f) => ({
          id: f.id,
          key: f.key,
          type: f.type as "STRING" | "NUMBER",
        })),
      );
    } catch (err) {
      console.error("Failed to fetch user context:", err);
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

  // Fetch user context once on mount
  useEffect(() => {
    void fetchUserContext();
  }, [fetchUserContext]);

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

      // Track schema sync event
      trackFeatureUsageClient(posthog, "schema_editor", {
        action: "schema_synced",
      });

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

    // Track table selection
    trackFeatureUsageClient(posthog, "schema_editor", {
      action: "table_selected",
    });
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

      // Track table changes
      if (payload.access) {
        trackFeatureUsageClient(posthog, "schema_editor", {
          action: "table_access_changed",
        });
      }
      if (payload.context !== undefined) {
        trackFeatureUsageClient(posthog, "schema_editor", {
          action: "table_prompt_edited",
        });
      }

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

      // Track column changes
      if (payload.selected !== undefined) {
        trackFeatureUsageClient(posthog, "schema_editor", {
          action: "column_selected",
        });
      }
      if (payload.rename !== undefined) {
        trackFeatureUsageClient(posthog, "schema_editor", {
          action: "column_renamed",
        });
      }
      if (payload.notes !== undefined) {
        trackFeatureUsageClient(posthog, "schema_editor", {
          action: "column_note_edited",
        });
      }

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

  // Computed column callbacks
  const onCreateComputedColumn = useCallback(
    async (
      tableId: string,
      payload: { name: string; ast: unknown; unit: string | null },
    ) => {
      const res = await fetch("/api/schema/computed-columns", {
        method: "POST",
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify({ tableId, ...payload }),
      });
      const data = (await res.json()) as { error?: string };
      if (data.error) throw new Error(data.error);

      // Track computed column creation
      trackFeatureUsageClient(posthog, "schema_editor", {
        action: "computed_column_created",
      });

      // Refresh table detail
      const table = tables.find((t) => t.id === tableId);
      if (table) {
        await fetchTableDetail(table.name);
      }
    },
    [tables, fetchTableDetail],
  );

  const onUpdateComputedColumn = useCallback(
    async (
      tableId: string,
      columnId: string,
      payload: { name?: string; selected?: boolean; notes?: string | null },
    ) => {
      const res = await fetch(`/api/schema/computed-columns/${columnId}`, {
        method: "PATCH",
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify(payload),
      });
      const data = (await res.json()) as { error?: string };
      if (data.error) throw new Error(data.error);

      // Track computed column update
      trackFeatureUsageClient(posthog, "schema_editor", {
        action: "computed_column_updated",
      });

      // Update local state optimistically
      if (selectedTable?.id === tableId) {
        setSelectedTable({
          ...selectedTable,
          computedColumns: selectedTable.computedColumns.map((cc) =>
            cc.id === columnId ? { ...cc, ...payload } : cc,
          ),
        });
      }
    },
    [selectedTable],
  );

  const onDeleteComputedColumn = useCallback(
    async (tableId: string, columnId: string) => {
      const res = await fetch(`/api/schema/computed-columns/${columnId}`, {
        method: "DELETE",
      });
      const data = (await res.json()) as { error?: string };
      if (data.error) throw new Error(data.error);

      // Track computed column deletion
      trackFeatureUsageClient(posthog, "schema_editor", {
        action: "computed_column_deleted",
      });

      // Update local state optimistically
      if (selectedTable?.id === tableId) {
        setSelectedTable({
          ...selectedTable,
          computedColumns: selectedTable.computedColumns.filter(
            (cc) => cc.id !== columnId,
          ),
        });
      }
    },
    [selectedTable],
  );

  // Manual relation callbacks
  const onCreateManualRelation = useCallback(
    async (
      tableId: string,
      payload: {
        name: string;
        isList: boolean;
        targetTableId: string;
        columnPairs: Array<{
          sourceColumnName: string;
          targetColumnName: string;
        }>;
      },
    ) => {
      const res = await fetch("/api/schema/manual-relations", {
        method: "POST",
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify({ sourceTableId: tableId, ...payload }),
      });
      const data = (await res.json()) as { error?: string };
      if (data.error) throw new Error(data.error);

      // Track manual relation creation
      trackFeatureUsageClient(posthog, "schema_editor", {
        action: "relation_created",
      });

      // Refresh table detail
      const table = tables.find((t) => t.id === tableId);
      if (table) {
        await fetchTableDetail(table.name);
      }
    },
    [tables, fetchTableDetail],
  );

  const onUpdateManualRelation = useCallback(
    async (
      tableId: string,
      relationId: string,
      payload: {
        name: string;
        isList: boolean;
        targetTableId: string;
        columnPairs: Array<{
          sourceColumnName: string;
          targetColumnName: string;
        }>;
      },
    ) => {
      const res = await fetch(`/api/schema/manual-relations/${relationId}`, {
        method: "PATCH",
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify(payload),
      });
      const data = (await res.json()) as { error?: string };
      if (data.error) throw new Error(data.error);

      // Track relation update
      trackFeatureUsageClient(posthog, "schema_editor", {
        action: "relation_updated",
      });

      // Refresh table detail
      const table = tables.find((t) => t.id === tableId);
      if (table) {
        await fetchTableDetail(table.name);
      }
    },
    [tables, fetchTableDetail],
  );

  const onDeleteManualRelation = useCallback(
    async (tableId: string, relationId: string) => {
      const res = await fetch(`/api/schema/manual-relations/${relationId}`, {
        method: "DELETE",
      });
      const data = (await res.json()) as { error?: string };
      if (data.error) throw new Error(data.error);

      // Track relation deletion
      trackFeatureUsageClient(posthog, "schema_editor", {
        action: "relation_deleted",
      });

      // Update local state optimistically
      if (selectedTable?.id === tableId) {
        setSelectedTable({
          ...selectedTable,
          relations: selectedTable.relations.filter(
            (rel) => rel.id !== relationId,
          ),
        });
      }
    },
    [selectedTable],
  );

  // Context filter callbacks
  const onUpsertContextFilter = useCallback(
    async (
      tableId: string,
      payload: { columnId: string; userContextFieldId: string },
    ) => {
      const table = tables.find((t) => t.id === tableId);
      if (!table) return;

      const res = await fetch(
        `/api/schema/tables/${encodeURIComponent(table.name)}/context-filter`,
        {
          method: "POST",
          headers: { "Content-Type": "application/json" },
          body: JSON.stringify(payload),
        },
      );
      const data = (await res.json()) as { error?: string };
      if (data.error) throw new Error(data.error);

      // Track context filter creation
      trackFeatureUsageClient(posthog, "schema_editor", {
        action: "context_field_created",
      });

      // Refresh table detail
      await fetchTableDetail(table.name);
    },
    [tables, fetchTableDetail],
  );

  const onDeleteContextFilter = useCallback(
    async (tableId: string) => {
      const table = tables.find((t) => t.id === tableId);
      if (!table) return;

      const res = await fetch(
        `/api/schema/tables/${encodeURIComponent(table.name)}/context-filter`,
        {
          method: "DELETE",
        },
      );
      const data = (await res.json()) as { error?: string };
      if (data.error) throw new Error(data.error);

      // Track context filter deletion
      trackFeatureUsageClient(posthog, "schema_editor", {
        action: "context_field_deleted",
      });

      // Refresh table detail
      await fetchTableDetail(table.name);
    },
    [tables, fetchTableDetail],
  );

  // Column conversion callbacks
  const onCreateColumnConversion = useCallback(
    async (
      tableId: string,
      columnId: string,
      payload: { type: string; ast: unknown; selected: boolean },
    ) => {
      const res = await fetch("/api/schema/conversions", {
        method: "POST",
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify({ columnId, ...payload }),
      });
      const data = (await res.json()) as { error?: string };
      if (data.error) throw new Error(data.error);

      // Refresh table detail
      const table = tables.find((t) => t.id === tableId);
      if (table) {
        await fetchTableDetail(table.name);
      }
    },
    [tables, fetchTableDetail],
  );

  const onUpdateColumnConversion = useCallback(
    async (
      tableId: string,
      columnId: string,
      payload: { type: string; ast: unknown; selected: boolean },
    ) => {
      // Find conversion ID from current state
      const column = selectedTable?.columns.find((c) => c.id === columnId);
      if (!column?.conversion?.id) {
        throw new Error("Conversion not found");
      }

      const res = await fetch(
        `/api/schema/conversions/${column.conversion.id}`,
        {
          method: "PATCH",
          headers: { "Content-Type": "application/json" },
          body: JSON.stringify(payload),
        },
      );
      const data = (await res.json()) as { error?: string };
      if (data.error) throw new Error(data.error);

      // Refresh table detail
      const table = tables.find((t) => t.id === tableId);
      if (table) {
        await fetchTableDetail(table.name);
      }
    },
    [selectedTable, tables, fetchTableDetail],
  );

  const onDeleteColumnConversion = useCallback(
    async (tableId: string, columnId: string) => {
      // Find conversion ID from current state
      const column = selectedTable?.columns.find((c) => c.id === columnId);
      if (!column?.conversion?.id) {
        throw new Error("Conversion not found");
      }

      const res = await fetch(
        `/api/schema/conversions/${column.conversion.id}`,
        {
          method: "DELETE",
        },
      );
      const data = (await res.json()) as { error?: string };
      if (data.error) throw new Error(data.error);

      // Update local state optimistically
      if (selectedTable?.id === tableId) {
        setSelectedTable({
          ...selectedTable,
          columns: selectedTable.columns.map((c) =>
            c.id === columnId ? { ...c, conversion: null } : c,
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

  // Available tables for manual relations - fetch all tables with their columns
  const [availableTables, setAvailableTables] = useState<TableWithColumns[]>(
    [],
  );

  const fetchAvailableTables = useCallback(async () => {
    try {
      // Fetch all tables (not filtered)
      const res = await fetch("/api/schema/tables?access=QUERYABLE,JOINABLE");
      const data = (await res.json()) as {
        tables?: ApiTableId[];
      };

      // Fetch column details for each table
      const tablesWithColumns: TableWithColumns[] = await Promise.all(
        (data.tables ?? []).map(async (t) => {
          const detailRes = await fetch(
            `/api/schema/tables/${encodeURIComponent(t.name)}`,
          );
          const detailData = (await detailRes.json()) as {
            table?: ApiTableDetail;
          };
          return {
            id: t.id,
            name: t.name,
            columns: (detailData.table?.columns ?? []).map((c) => ({
              id: c.id,
              name: c.name,
              type: c.type,
            })),
          };
        }),
      );

      setAvailableTables(tablesWithColumns);
    } catch (err) {
      console.error("Failed to fetch available tables:", err);
    }
  }, []);

  // Fetch available tables once on mount
  useEffect(() => {
    void fetchAvailableTables();
  }, [fetchAvailableTables]);

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
          userContextFields={userContextFields}
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
          // Computed column callbacks
          onCreateComputedColumn={onCreateComputedColumn}
          onUpdateComputedColumn={onUpdateComputedColumn}
          onDeleteComputedColumn={onDeleteComputedColumn}
          // Manual relation callbacks
          onCreateManualRelation={onCreateManualRelation}
          onUpdateManualRelation={onUpdateManualRelation}
          onDeleteManualRelation={onDeleteManualRelation}
          // Context filter callbacks
          onUpsertContextFilter={onUpsertContextFilter}
          onDeleteContextFilter={onDeleteContextFilter}
          // Column conversion callbacks
          onCreateColumnConversion={onCreateColumnConversion}
          onUpdateColumnConversion={onUpdateColumnConversion}
          onDeleteColumnConversion={onDeleteColumnConversion}
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
