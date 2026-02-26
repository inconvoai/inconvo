"use client";

import { useState, useCallback, type ReactNode } from "react";
import { Flex, Box, Text, Center, Stack } from "@mantine/core";
import { IconTable } from "@tabler/icons-react";
import type {
  TableSummary,
  TableSchema,
  TableAccess,
  UserContextField,
  UserContextStatus,
  TableWithColumns,
  UpdateTablePayload,
  ColumnUpdatePayload,
  ComputedColumnCreatePayload,
  ComputedColumnUpdatePayload,
  ManualRelationCreatePayload,
  ManualRelationUpdatePayload,
  ContextFilterPayload,
  UpsertTableAccessPolicyPayload,
  ColumnConversionCreatePayload,
  ColumnConversionUpdatePayload,
  ColumnValueEnumCreatePayload,
  ColumnValueEnumEntryInput,
  ColumnValueEnumUpdatePayload,
  ColumnUnitPayload,
  ComputedColumnUnitPayload,
  CreateVirtualTablePayload,
  UpdateVirtualTableSqlPayload,
  VirtualTableValidationResult,
  VirtualTableColumnRefreshResult,
} from "./types";
import {
  TableList,
  type FilterValue,
  type ConnectionOption,
} from "./TableList";
import { TableDetail } from "./TableDetail";

export type { FilterValue };

export interface SemanticModelEditorProps {
  /** List of table summaries for the left panel */
  tables: TableSummary[];
  /** Currently selected table (full details) */
  selectedTable: TableSchema | null;
  /** User context fields for access constraints */
  userContextFields: UserContextField[];
  /** User context status for access constraints */
  userContextStatus?: UserContextStatus;
  /** Available tables for manual relations */
  availableTables: TableWithColumns[];
  /** Whether the table list is loading */
  listLoading?: boolean;
  /** Whether the selected table is loading */
  detailLoading?: boolean;
  /** Whether the editor is in read-only mode (disables all mutations) */
  readOnly?: boolean;
  /** Callback when a table is selected */
  onTableSelect: (tableId: string) => void;

  // Server-side filtering/pagination props (optional - if not provided, uses client-side)
  /** Search query (controlled, for server-side search) */
  searchQuery?: string;
  /** Callback when search changes */
  onSearchChange?: (query: string) => void;
  /** Access filter (controlled) */
  accessFilter?: FilterValue;
  /** Callback when access filter changes */
  onAccessFilterChange?: (filter: FilterValue) => void;
  /** Total table count for pagination */
  totalTableCount?: number;
  /** Current page (1-indexed) */
  currentPage?: number;
  /** Callback when page changes */
  onPageChange?: (page: number) => void;
  /** Items per page */
  perPage?: number;

  // Connection selector props (for multi-database support)
  /** Available connections */
  connections?: ConnectionOption[];
  /** Currently selected connection ID */
  selectedConnectionId?: string | null;
  /** Callback when connection changes */
  onConnectionChange?: (connectionId: string | null) => void;
  /** Optional content to render in the detail pane when no table is selected */
  emptyStateContent?: ReactNode;
  /** Callback when table is updated */
  onUpdateTable?: (
    tableId: string,
    payload: UpdateTablePayload,
  ) => Promise<void>;
  /** Callback when a column is updated */
  onUpdateColumn?: (
    tableId: string,
    columnId: string,
    payload: ColumnUpdatePayload,
  ) => Promise<void>;
  /** Callback when a computed column is created */
  onCreateComputedColumn?: (
    tableId: string,
    payload: ComputedColumnCreatePayload,
  ) => Promise<void>;
  /** Callback when a computed column is updated */
  onUpdateComputedColumn?: (
    tableId: string,
    columnId: string,
    payload: ComputedColumnUpdatePayload,
  ) => Promise<void>;
  /** Callback when a computed column is deleted */
  onDeleteComputedColumn?: (tableId: string, columnId: string) => Promise<void>;
  /** Callback when a relation is updated */
  onUpdateRelation?: (
    tableId: string,
    relationId: string,
    selected: boolean,
  ) => Promise<void>;
  /** Callback when a manual relation is created */
  onCreateManualRelation?: (
    tableId: string,
    payload: ManualRelationCreatePayload,
  ) => Promise<void>;
  /** Callback when a manual relation is updated */
  onUpdateManualRelation?: (
    tableId: string,
    relationId: string,
    payload: ManualRelationUpdatePayload,
  ) => Promise<void>;
  /** Callback when a manual relation is deleted */
  onDeleteManualRelation?: (
    tableId: string,
    relationId: string,
  ) => Promise<void>;
  /** Callback when a context filter is created/updated */
  onUpsertContextFilter?: (
    tableId: string,
    payload: ContextFilterPayload,
  ) => Promise<void>;
  /** Callback when a context filter is deleted */
  onDeleteContextFilter?: (tableId: string) => Promise<void>;
  /** Callback when a table access policy is created/updated */
  onUpsertTableAccessPolicy?: (
    tableId: string,
    payload: UpsertTableAccessPolicyPayload,
  ) => Promise<void>;
  /** Callback when a table access policy is deleted */
  onDeleteTableAccessPolicy?: (tableId: string) => Promise<void>;
  /** Callback when a column conversion is created */
  onCreateColumnConversion?: (
    tableId: string,
    columnId: string,
    payload: ColumnConversionCreatePayload,
  ) => Promise<void>;
  /** Callback when a column conversion is updated */
  onUpdateColumnConversion?: (
    tableId: string,
    columnId: string,
    payload: ColumnConversionUpdatePayload,
  ) => Promise<void>;
  /** Callback when a column conversion is deleted */
  onDeleteColumnConversion?: (
    tableId: string,
    columnId: string,
  ) => Promise<void>;
  /** Callback when a column value enum is created */
  onCreateColumnValueEnum?: (
    tableId: string,
    columnId: string,
    payload: ColumnValueEnumCreatePayload,
  ) => Promise<void>;
  /** Callback when a column value enum is updated */
  onUpdateColumnValueEnum?: (
    tableId: string,
    columnId: string,
    payload: ColumnValueEnumUpdatePayload,
  ) => Promise<void>;
  /** Callback when a column value enum is deleted */
  onDeleteColumnValueEnum?: (
    tableId: string,
    columnId: string,
  ) => Promise<void>;
  /** Callback to auto-generate enum entries from distinct values */
  onAutoFillColumnValueEnum?: (
    tableId: string,
    columnId: string,
  ) => Promise<ColumnValueEnumEntryInput[]>;
  /** Callback when a column unit is added */
  onAddColumnUnit?: (
    tableId: string,
    payload: ColumnUnitPayload,
  ) => Promise<void>;
  /** Callback when a computed column unit is updated */
  onUpdateComputedColumnUnit?: (
    tableId: string,
    payload: ComputedColumnUnitPayload,
  ) => Promise<void>;
  /** Callback when user requests creating a new virtual table (opens platform-specific UI) */
  onRequestCreateVirtualTable?: () => void;
  /** Validate virtual table SQL */
  onValidateVirtualTableSql?: (payload: {
    connectionId: string;
    sql: string;
    dialect?: CreateVirtualTablePayload["dialect"];
    previewLimit?: number;
  }) => Promise<VirtualTableValidationResult>;
  /** Create a new virtual table */
  onCreateVirtualTable?: (
    payload: CreateVirtualTablePayload,
  ) => Promise<{ tableId: string } | void>;
  /** Update SQL/config for an existing virtual table */
  onUpdateVirtualTableSql?: (
    tableId: string,
    payload: UpdateVirtualTableSqlPayload,
  ) => Promise<{ tableId: string } | void>;
  /** Refresh inferred columns from current SQL */
  onRefreshVirtualTableColumns?: (
    tableId: string,
  ) => Promise<VirtualTableColumnRefreshResult | void>;
  /** Delete a virtual table */
  onDeleteVirtualTable?: (tableId: string) => Promise<void>;
}

export function SemanticModelEditor({
  tables,
  selectedTable,
  userContextFields,
  userContextStatus,
  availableTables,
  listLoading = false,
  detailLoading = false,
  readOnly = false,
  onTableSelect,
  // Server-side filtering props
  searchQuery: controlledSearchQuery,
  onSearchChange,
  accessFilter: controlledAccessFilter,
  onAccessFilterChange,
  totalTableCount,
  currentPage,
  onPageChange,
  perPage,
  // Connection selector props
  connections,
  selectedConnectionId,
  onConnectionChange,
  emptyStateContent,
  // Mutation callbacks
  onUpdateTable,
  onUpdateColumn,
  onCreateComputedColumn,
  onUpdateComputedColumn,
  onDeleteComputedColumn,
  onUpdateRelation,
  onCreateManualRelation,
  onUpdateManualRelation,
  onDeleteManualRelation,
  onUpsertContextFilter,
  onDeleteContextFilter,
  onUpsertTableAccessPolicy,
  onDeleteTableAccessPolicy,
  onCreateColumnConversion,
  onUpdateColumnConversion,
  onDeleteColumnConversion,
  onCreateColumnValueEnum,
  onUpdateColumnValueEnum,
  onDeleteColumnValueEnum,
  onAutoFillColumnValueEnum,
  onAddColumnUnit,
  onUpdateComputedColumnUnit,
  onRequestCreateVirtualTable,
  onValidateVirtualTableSql,
  onCreateVirtualTable,
  onUpdateVirtualTableSql,
  onRefreshVirtualTableColumns,
  onDeleteVirtualTable,
}: SemanticModelEditorProps) {
  // Internal state for client-side mode (when server-side callbacks not provided)
  const [internalSearchQuery, setInternalSearchQuery] = useState("");
  const [internalAccessFilter, setInternalAccessFilter] =
    useState<FilterValue>("active");

  // Use controlled or internal state
  const searchQuery = controlledSearchQuery ?? internalSearchQuery;
  const accessFilter = controlledAccessFilter ?? internalAccessFilter;

  const handleSearchChange = onSearchChange ?? setInternalSearchQuery;
  const handleAccessFilterChange =
    onAccessFilterChange ?? setInternalAccessFilter;

  // Handler for changing table access from sidebar
  const handleTableAccessChange = useCallback(
    async (tableId: string, access: TableAccess) => {
      await onUpdateTable?.(tableId, { access });
    },
    [onUpdateTable],
  );

  // Wrap callbacks to include tableId
  const handleUpdateTable = useCallback(
    async (payload: UpdateTablePayload) => {
      if (selectedTable) {
        await onUpdateTable?.(selectedTable.id, payload);
      }
    },
    [selectedTable, onUpdateTable],
  );

  const handleUpdateColumn = useCallback(
    async (columnId: string, payload: ColumnUpdatePayload) => {
      if (selectedTable) {
        await onUpdateColumn?.(selectedTable.id, columnId, payload);
      }
    },
    [selectedTable, onUpdateColumn],
  );

  const handleCreateComputedColumn = useCallback(
    async (payload: ComputedColumnCreatePayload) => {
      if (selectedTable) {
        await onCreateComputedColumn?.(selectedTable.id, payload);
      }
    },
    [selectedTable, onCreateComputedColumn],
  );

  const handleUpdateComputedColumn = useCallback(
    async (columnId: string, payload: ComputedColumnUpdatePayload) => {
      if (selectedTable) {
        await onUpdateComputedColumn?.(selectedTable.id, columnId, payload);
      }
    },
    [selectedTable, onUpdateComputedColumn],
  );

  const handleDeleteComputedColumn = useCallback(
    async (columnId: string) => {
      if (selectedTable) {
        await onDeleteComputedColumn?.(selectedTable.id, columnId);
      }
    },
    [selectedTable, onDeleteComputedColumn],
  );

  const handleUpdateRelation = useCallback(
    async (relationId: string, selected: boolean) => {
      if (selectedTable) {
        await onUpdateRelation?.(selectedTable.id, relationId, selected);
      }
    },
    [selectedTable, onUpdateRelation],
  );

  const handleCreateManualRelation = useCallback(
    async (payload: ManualRelationCreatePayload) => {
      if (selectedTable) {
        await onCreateManualRelation?.(selectedTable.id, payload);
      }
    },
    [selectedTable, onCreateManualRelation],
  );

  const handleUpdateManualRelation = useCallback(
    async (relationId: string, payload: ManualRelationUpdatePayload) => {
      if (selectedTable) {
        await onUpdateManualRelation?.(selectedTable.id, relationId, payload);
      }
    },
    [selectedTable, onUpdateManualRelation],
  );

  const handleDeleteManualRelation = useCallback(
    async (relationId: string) => {
      if (selectedTable) {
        await onDeleteManualRelation?.(selectedTable.id, relationId);
      }
    },
    [selectedTable, onDeleteManualRelation],
  );

  const handleUpsertContextFilter = useCallback(
    async (payload: ContextFilterPayload) => {
      if (selectedTable) {
        await onUpsertContextFilter?.(selectedTable.id, payload);
      }
    },
    [selectedTable, onUpsertContextFilter],
  );

  const handleDeleteContextFilter = useCallback(async () => {
    if (selectedTable) {
      await onDeleteContextFilter?.(selectedTable.id);
    }
  }, [selectedTable, onDeleteContextFilter]);

  const handleUpsertTableAccessPolicy = useCallback(
    async (payload: UpsertTableAccessPolicyPayload) => {
      if (selectedTable) {
        await onUpsertTableAccessPolicy?.(selectedTable.id, payload);
      }
    },
    [selectedTable, onUpsertTableAccessPolicy],
  );

  const handleDeleteTableAccessPolicy = useCallback(async () => {
    if (selectedTable) {
      await onDeleteTableAccessPolicy?.(selectedTable.id);
    }
  }, [selectedTable, onDeleteTableAccessPolicy]);

  const handleCreateColumnConversion = useCallback(
    async (columnId: string, payload: ColumnConversionCreatePayload) => {
      if (selectedTable) {
        await onCreateColumnConversion?.(selectedTable.id, columnId, payload);
      }
    },
    [selectedTable, onCreateColumnConversion],
  );

  const handleUpdateColumnConversion = useCallback(
    async (columnId: string, payload: ColumnConversionUpdatePayload) => {
      if (selectedTable) {
        await onUpdateColumnConversion?.(selectedTable.id, columnId, payload);
      }
    },
    [selectedTable, onUpdateColumnConversion],
  );

  const handleDeleteColumnConversion = useCallback(
    async (columnId: string) => {
      if (selectedTable) {
        await onDeleteColumnConversion?.(selectedTable.id, columnId);
      }
    },
    [selectedTable, onDeleteColumnConversion],
  );

  const handleCreateColumnValueEnum = useCallback(
    async (columnId: string, payload: ColumnValueEnumCreatePayload) => {
      if (selectedTable) {
        await onCreateColumnValueEnum?.(selectedTable.id, columnId, payload);
      }
    },
    [selectedTable, onCreateColumnValueEnum],
  );

  const handleUpdateColumnValueEnum = useCallback(
    async (columnId: string, payload: ColumnValueEnumUpdatePayload) => {
      if (selectedTable) {
        await onUpdateColumnValueEnum?.(selectedTable.id, columnId, payload);
      }
    },
    [selectedTable, onUpdateColumnValueEnum],
  );

  const handleDeleteColumnValueEnum = useCallback(
    async (columnId: string) => {
      if (selectedTable) {
        await onDeleteColumnValueEnum?.(selectedTable.id, columnId);
      }
    },
    [selectedTable, onDeleteColumnValueEnum],
  );

  const handleAutoFillColumnValueEnum = useCallback(
    async (columnId: string) => {
      if (!selectedTable || !onAutoFillColumnValueEnum) {
        return [];
      }
      return await onAutoFillColumnValueEnum(selectedTable.id, columnId);
    },
    [selectedTable, onAutoFillColumnValueEnum],
  );

  const handleAddColumnUnit = useCallback(
    async (payload: ColumnUnitPayload) => {
      if (selectedTable) {
        await onAddColumnUnit?.(selectedTable.id, payload);
      }
    },
    [selectedTable, onAddColumnUnit],
  );

  const handleUpdateComputedColumnUnit = useCallback(
    async (payload: ComputedColumnUnitPayload) => {
      if (selectedTable) {
        await onUpdateComputedColumnUnit?.(selectedTable.id, payload);
      }
    },
    [selectedTable, onUpdateComputedColumnUnit],
  );

  const resolvedUserContextStatus = userContextStatus ?? "UNSET";

  return (
    <Flex h="100%" gap={0}>
      {/* Left Sidebar - Table List */}
      <Box
        w={300}
        bg="gray.0"
        style={{
          borderRight: "1px solid var(--mantine-color-gray-3)",
          display: "flex",
          flexDirection: "column",
          height: "100%",
        }}
      >
        <TableList
          tables={tables}
          selectedTableId={selectedTable?.id ?? null}
          onTableSelect={onTableSelect}
          onTableAccessChange={handleTableAccessChange}
          loading={listLoading}
          searchQuery={searchQuery}
          onSearchChange={handleSearchChange}
          accessFilter={accessFilter}
          onAccessFilterChange={handleAccessFilterChange}
          totalCount={totalTableCount}
          currentPage={currentPage}
          onPageChange={onPageChange}
          perPage={perPage}
          connections={connections}
          selectedConnectionId={selectedConnectionId}
          onConnectionChange={onConnectionChange}
          userContextStatus={resolvedUserContextStatus}
          onCreateVirtualTable={onRequestCreateVirtualTable}
          disableCreateVirtualTable={readOnly}
        />
      </Box>

      {/* Right Panel - Table Details */}
      <Box
        style={{
          flex: 1,
          display: "flex",
          flexDirection: "column",
          minWidth: 0,
          height: "100%",
        }}
        bg="white"
      >
        {selectedTable ? (
          <TableDetail
            table={selectedTable}
            userContextFields={userContextFields}
            userContextStatus={resolvedUserContextStatus}
            availableTables={availableTables}
            loading={detailLoading}
            readOnly={readOnly}
            onUpdateTable={handleUpdateTable}
            onUpdateColumn={handleUpdateColumn}
            onCreateComputedColumn={handleCreateComputedColumn}
            onUpdateComputedColumn={handleUpdateComputedColumn}
            onDeleteComputedColumn={handleDeleteComputedColumn}
            onUpdateRelation={handleUpdateRelation}
            onCreateManualRelation={handleCreateManualRelation}
            onUpdateManualRelation={handleUpdateManualRelation}
            onDeleteManualRelation={handleDeleteManualRelation}
            onUpsertContextFilter={handleUpsertContextFilter}
            onDeleteContextFilter={handleDeleteContextFilter}
            onUpsertTableAccessPolicy={handleUpsertTableAccessPolicy}
            onDeleteTableAccessPolicy={handleDeleteTableAccessPolicy}
            onCreateColumnConversion={handleCreateColumnConversion}
            onUpdateColumnConversion={handleUpdateColumnConversion}
            onDeleteColumnConversion={handleDeleteColumnConversion}
            onCreateColumnValueEnum={handleCreateColumnValueEnum}
            onUpdateColumnValueEnum={handleUpdateColumnValueEnum}
            onDeleteColumnValueEnum={handleDeleteColumnValueEnum}
            onAutoFillColumnValueEnum={handleAutoFillColumnValueEnum}
            onAddColumnUnit={handleAddColumnUnit}
            onUpdateComputedColumnUnit={handleUpdateComputedColumnUnit}
            selectedConnectionId={selectedConnectionId}
            onValidateVirtualTableSql={onValidateVirtualTableSql}
            onUpdateVirtualTableSql={onUpdateVirtualTableSql}
            onRefreshVirtualTableColumns={onRefreshVirtualTableColumns}
            onDeleteVirtualTable={onDeleteVirtualTable}
          />
        ) : (
          emptyStateContent ?? (
            <Center h="100%">
              <Stack align="center" gap="xs">
                <IconTable size={48} opacity={0.3} />
                <Text c="dimmed">Select a table to view details</Text>
              </Stack>
            </Center>
          )
        )}
      </Box>
    </Flex>
  );
}
