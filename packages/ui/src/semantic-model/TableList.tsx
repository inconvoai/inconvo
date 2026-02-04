"use client";

import { useState } from "react";
import {
  TextInput,
  SegmentedControl,
  Group,
  Text,
  Box,
  Stack,
  Badge,
  ActionIcon,
  ScrollArea,
  Pagination,
  Center,
  Loader,
  Select,
  rem,
} from "@mantine/core";
import {
  IconSearch,
  IconCirclesRelation,
  IconCircleOff,
  IconX,
  IconLock,
  IconLockOff,
  IconLockOpen,
  IconAlertCircle,
  IconDatabase,
} from "@tabler/icons-react";
import { useDebouncedCallback } from "@mantine/hooks";
import type { TableSummary, TableAccess, UserContextStatus } from "./types";
import { AccessControl } from "./AccessControl";

export type FilterValue = "active" | "queryable" | "joinable" | "off" | "all";

export interface ConnectionOption {
  id: string;
  name: string;
}

export interface TableListProps {
  /** List of tables to display */
  tables: TableSummary[];
  /** Currently selected table ID */
  selectedTableId: string | null;
  /** Callback when a table is selected */
  onTableSelect: (tableId: string) => void;
  /** Callback when a table's access level changes */
  onTableAccessChange?: (tableId: string, access: TableAccess) => void;
  /** Whether the list is loading */
  loading?: boolean;
  /** Search query (controlled) */
  searchQuery?: string;
  /** Callback when search query changes - if provided, search is server-side */
  onSearchChange?: (query: string) => void;
  /** Access filter (controlled) */
  accessFilter?: FilterValue;
  /** Callback when access filter changes - if provided, filter is server-side */
  onAccessFilterChange?: (filter: FilterValue) => void;
  /** Total count of tables (for pagination) */
  totalCount?: number;
  /** Current page (1-indexed) */
  currentPage?: number;
  /** Callback when page changes */
  onPageChange?: (page: number) => void;
  /** Items per page */
  perPage?: number;
  /** Available connections for multi-database support */
  connections?: ConnectionOption[];
  /** Currently selected connection ID */
  selectedConnectionId?: string | null;
  /** Callback when connection changes */
  onConnectionChange?: (connectionId: string | null) => void;
  /** User context status for access constraints */
  userContextStatus?: UserContextStatus;
}

function getAccessColor(access: TableAccess): string {
  switch (access) {
    case "QUERYABLE":
      return "green";
    case "JOINABLE":
      return "orange";
    case "OFF":
      return "gray";
    default:
      return "gray";
  }
}

function getAccessIcon(access: TableAccess) {
  switch (access) {
    case "QUERYABLE":
      return <IconSearch size={12} />;
    case "JOINABLE":
      return <IconCirclesRelation size={12} />;
    case "OFF":
      return <IconCircleOff size={12} />;
    default:
      return null;
  }
}

export function TableList({
  tables,
  selectedTableId,
  onTableSelect,
  onTableAccessChange,
  loading = false,
  searchQuery: controlledSearchQuery,
  onSearchChange,
  accessFilter: controlledAccessFilter,
  onAccessFilterChange,
  totalCount,
  currentPage = 1,
  onPageChange,
  perPage = 20,
  connections,
  selectedConnectionId,
  onConnectionChange,
  userContextStatus = "UNSET",
}: TableListProps) {
  // Internal state for uncontrolled mode
  const [internalSearchQuery, setInternalSearchQuery] = useState("");
  const [internalAccessFilter, setInternalAccessFilter] =
    useState<FilterValue>("active");
  const [inputValue, setInputValue] = useState(controlledSearchQuery ?? "");

  // Use controlled or internal state
  const searchQuery = controlledSearchQuery ?? internalSearchQuery;
  const accessFilter = controlledAccessFilter ?? internalAccessFilter;

  // Determine if we're in server-side mode (callbacks provided)
  const isServerSide = !!onSearchChange && !!onAccessFilterChange;

  const handleSearchChange = (value: string) => {
    if (onSearchChange) {
      onSearchChange(value);
    } else {
      setInternalSearchQuery(value);
    }
  };

  const handleAccessFilterChange = (value: string) => {
    const filterValue = value as FilterValue;
    if (onAccessFilterChange) {
      onAccessFilterChange(filterValue);
    } else {
      setInternalAccessFilter(filterValue);
    }
  };

  const debouncedSearch = useDebouncedCallback(handleSearchChange, 300);

  const handleInputChange = (value: string) => {
    setInputValue(value);
    debouncedSearch(value);
  };

  const clearSearch = () => {
    setInputValue("");
    handleSearchChange("");
  };

  // Apply client-side filtering only if not in server-side mode
  const displayTables = isServerSide
    ? tables
    : tables.filter((table) => {
        // Search filter
        if (searchQuery) {
          const query = searchQuery.toLowerCase();
          if (!table.name.toLowerCase().includes(query)) {
            return false;
          }
        }
        // Access filter
        switch (accessFilter) {
          case "active":
            return table.access === "QUERYABLE" || table.access === "JOINABLE";
          case "queryable":
            return table.access === "QUERYABLE";
          case "joinable":
            return table.access === "JOINABLE";
          case "off":
            return table.access === "OFF";
          case "all":
          default:
            return true;
        }
      });

  // Calculate pagination
  const totalPages = totalCount ? Math.ceil(totalCount / perPage) : 1;
  const showPagination = isServerSide && totalCount && totalCount > perPage;

  // Connection selector options
  const connectionOptions = connections?.map((c) => ({
    value: c.id,
    label: c.name,
  }));
  const showConnectionSelector = connections && connections.length >= 1;
  const accessConstraintLabel =
    userContextStatus === "ENABLED"
      ? "On"
      : userContextStatus === "DISABLED"
        ? "Off"
        : "Not configured";
  const accessConstraintIcon =
    userContextStatus === "ENABLED" ? (
      <IconLock size={14} color="var(--mantine-color-blue-6)" />
    ) : userContextStatus === "DISABLED" ? (
      <IconLockOff size={14} color="var(--mantine-color-gray-6)" />
    ) : (
      <IconAlertCircle size={14} color="var(--mantine-color-gray-6)" />
    );

  return (
    <Stack gap="sm" h="100%" p="sm">
      {/* Connection selector */}
      {showConnectionSelector && (
        <Select
          leftSection={<IconDatabase size={16} />}
          placeholder="Select database"
          data={connectionOptions}
          value={selectedConnectionId ?? null}
          onChange={onConnectionChange}
          size="sm"
        />
      )}

      {/* Search */}
      <TextInput
        placeholder="Search tables..."
        value={inputValue}
        onChange={(e) => handleInputChange(e.currentTarget.value)}
        rightSection={
          inputValue ? (
            <ActionIcon
              variant="subtle"
              radius="xl"
              size="sm"
              onClick={clearSearch}
            >
              <IconX style={{ width: rem(16), height: rem(16) }} />
            </ActionIcon>
          ) : (
            <IconSearch
              style={{ width: rem(16), height: rem(16) }}
              opacity={0.5}
            />
          )
        }
      />

      {/* Filter */}
      <SegmentedControl
        value={accessFilter}
        onChange={handleAccessFilterChange}
        size="xs"
        data={[
          { label: "Active", value: "active" },
          { label: "Query", value: "queryable" },
          { label: "Join", value: "joinable" },
          { label: "Off", value: "off" },
          { label: "All", value: "all" },
        ]}
      />

      <Group gap="xs">
        {accessConstraintIcon}
        <Text size="xs" c="dimmed">
          Access constraints: {accessConstraintLabel}
        </Text>
      </Group>

      {/* Table list */}
      <ScrollArea style={{ flex: 1 }}>
        {loading ? (
          <Center py="xl">
            <Loader size="sm" />
          </Center>
        ) : displayTables.length === 0 ? (
          <Text c="dimmed" ta="center" py="md" size="sm">
            {searchQuery
              ? `No tables matching "${searchQuery}"`
              : "No tables match the current filter"}
          </Text>
        ) : (
          <Stack gap={2}>
            {displayTables.map((table) => (
              <Box
                key={table.id}
                onClick={() => onTableSelect(table.id)}
                style={{
                  padding: "8px 12px",
                  borderRadius: "var(--mantine-radius-sm)",
                  cursor: "pointer",
                  backgroundColor:
                    selectedTableId === table.id
                      ? "var(--mantine-color-blue-light)"
                      : "transparent",
                }}
                onMouseEnter={(e) => {
                  if (selectedTableId !== table.id) {
                    e.currentTarget.style.backgroundColor =
                      "var(--mantine-color-gray-1)";
                  }
                }}
                onMouseLeave={(e) => {
                  if (selectedTableId !== table.id) {
                    e.currentTarget.style.backgroundColor = "transparent";
                  }
                }}
              >
                <Group justify="space-between" wrap="nowrap">
                  <Group gap="xs" wrap="nowrap" style={{ overflow: "hidden" }}>
                    <Text
                      fw={selectedTableId === table.id ? 600 : 400}
                      size="sm"
                      truncate
                    >
                      {table.name}
                    </Text>
                    {userContextStatus === "ENABLED" ? (
                      <Box
                        title={
                          table.hasCondition
                            ? "Access constraint active"
                            : "No access constraint"
                        }
                      >
                        {table.hasCondition ? (
                          <IconLock
                            size={12}
                            color="var(--mantine-color-blue-6)"
                          />
                        ) : (
                          <IconLockOpen
                            size={12}
                            color="var(--mantine-color-gray-6)"
                          />
                        )}
                      </Box>
                    ) : (
                      table.hasCondition && (
                        <Box title="Access constraint inactive">
                          <IconLockOff
                            size={12}
                            color="var(--mantine-color-gray-6)"
                          />
                        </Box>
                      )
                    )}
                  </Group>
                  {onTableAccessChange ? (
                    <AccessControl
                      value={table.access}
                      onChange={(access) =>
                        onTableAccessChange(table.id, access)
                      }
                      size="xs"
                      showLabels={false}
                    />
                  ) : (
                    <Badge
                      size="xs"
                      color={getAccessColor(table.access)}
                      variant="light"
                      leftSection={getAccessIcon(table.access)}
                    >
                      {table.access.charAt(0)}
                    </Badge>
                  )}
                </Group>
              </Box>
            ))}
          </Stack>
        )}
      </ScrollArea>

      {/* Pagination */}
      {showPagination && (
        <Center>
          <Pagination
            size="xs"
            total={totalPages}
            value={currentPage}
            onChange={onPageChange}
          />
        </Center>
      )}
    </Stack>
  );
}
