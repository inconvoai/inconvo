"use client";

import { useState } from "react";
import {
  Table,
  Checkbox,
  Badge,
  Group,
  Text,
  ActionIcon,
  Tooltip,
  Collapse,
  TextInput,
  Box,
  Menu,
} from "@mantine/core";
import {
  IconChevronUp,
  IconChevronDown,
  IconEdit,
  IconCheck,
  IconX,
  IconNotes,
  IconCashEdit,
  IconDotsVertical,
  IconSearch,
  IconTrash,
} from "@tabler/icons-react";
import type { Column, ComputedColumn } from "./types";

export interface ColumnTableProps {
  /** Regular columns */
  columns: Column[];
  /** Computed columns */
  computedColumns: ComputedColumn[];
  /** Whether the table is disabled (access = OFF) */
  disabled?: boolean;
  /** Callback when a column's selected state changes */
  onColumnSelectedChange?: (columnId: string, selected: boolean) => void;
  /** Callback when a column is renamed */
  onColumnRename?: (columnId: string, newName: string | null) => void;
  /** Callback when column notes action is clicked */
  onColumnNotesClick?: (column: Column) => void;
  /** Callback when column conversion action is clicked */
  onColumnConversionClick?: (column: Column) => void;
  /** Callback when a computed column's selected state changes */
  onComputedColumnSelectedChange?: (
    columnId: string,
    selected: boolean,
  ) => void;
  /** Callback when a computed column is renamed */
  onComputedColumnRename?: (columnId: string, newName: string) => void;
  /** Callback when computed column notes action is clicked */
  onComputedColumnNotesClick?: (column: ComputedColumn) => void;
  /** Callback when computed column delete is clicked */
  onComputedColumnDelete?: (columnId: string) => void;
}

export function ColumnTable({
  columns,
  computedColumns,
  disabled = false,
  onColumnSelectedChange,
  onColumnRename,
  onColumnNotesClick,
  onColumnConversionClick,
  onComputedColumnSelectedChange,
  onComputedColumnRename,
  onComputedColumnNotesClick,
  onComputedColumnDelete: _onComputedColumnDelete,
}: ColumnTableProps) {
  const [activeExpanded, setActiveExpanded] = useState(true);
  const [inactiveExpanded, setInactiveExpanded] = useState(false);
  const [editingColumnId, setEditingColumnId] = useState<string | null>(null);
  const [editingValue, setEditingValue] = useState("");
  const [editingComputedColumnId, setEditingComputedColumnId] = useState<
    string | null
  >(null);
  const [editingComputedValue, setEditingComputedValue] = useState("");
  const [filterValue, setFilterValue] = useState("");

  // Filter function for columns
  const matchesFilter = (name: string, rename?: string | null) => {
    if (!filterValue) return true;
    const search = filterValue.toLowerCase();
    return (
      name.toLowerCase().includes(search) ||
      (rename?.toLowerCase().includes(search) ?? false)
    );
  };

  const activeColumns = columns
    .filter((col) => col.selected)
    .filter((col) => matchesFilter(col.name, col.rename));
  const inactiveColumns = columns
    .filter((col) => !col.selected)
    .filter((col) => matchesFilter(col.name, col.rename));
  const activeComputedColumns = computedColumns
    .filter((col) => col.selected)
    .filter((col) => matchesFilter(col.name));
  const inactiveComputedColumns = computedColumns
    .filter((col) => !col.selected)
    .filter((col) => matchesFilter(col.name));

  const startEditingColumn = (column: Column) => {
    setEditingColumnId(column.id);
    setEditingValue(column.rename ?? column.name);
  };

  const cancelEditingColumn = () => {
    setEditingColumnId(null);
    setEditingValue("");
  };

  const submitColumnRename = (column: Column) => {
    const trimmedValue = editingValue.trim();
    const newRename =
      trimmedValue === column.name ? null : trimmedValue || null;
    onColumnRename?.(column.id, newRename);
    cancelEditingColumn();
  };

  const startEditingComputedColumn = (column: ComputedColumn) => {
    setEditingComputedColumnId(column.id);
    setEditingComputedValue(column.name);
  };

  const cancelEditingComputedColumn = () => {
    setEditingComputedColumnId(null);
    setEditingComputedValue("");
  };

  const submitComputedColumnRename = (column: ComputedColumn) => {
    const trimmedValue = editingComputedValue.trim();
    if (trimmedValue && trimmedValue !== column.name) {
      onComputedColumnRename?.(column.id, trimmedValue);
    }
    cancelEditingComputedColumn();
  };

  const renderColumnRow = (column: Column) => {
    const isEditing = editingColumnId === column.id;
    const displayName = column.rename ?? column.name;
    const hasRename = column.rename && column.rename !== column.name;
    const isStringType = column.type.toLowerCase().includes("string");

    return (
      <Table.Tr key={column.id}>
        <Table.Td>
          <Checkbox
            disabled={disabled}
            checked={column.selected}
            onChange={(event) =>
              onColumnSelectedChange?.(column.id, event.currentTarget.checked)
            }
          />
        </Table.Td>
        <Table.Td>
          {isEditing ? (
            <Group gap="xs">
              <TextInput
                size="xs"
                value={editingValue}
                onChange={(e) => setEditingValue(e.currentTarget.value)}
                onKeyDown={(e) => {
                  if (e.key === "Enter") submitColumnRename(column);
                  if (e.key === "Escape") cancelEditingColumn();
                }}
                autoFocus
                style={{ flex: 1, minWidth: 150 }}
              />
              <ActionIcon
                size="sm"
                color="green"
                variant="light"
                onClick={() => submitColumnRename(column)}
              >
                <IconCheck size={14} />
              </ActionIcon>
              <ActionIcon
                size="sm"
                color="gray"
                variant="light"
                onClick={cancelEditingColumn}
              >
                <IconX size={14} />
              </ActionIcon>
            </Group>
          ) : (
            <Group gap="xs" wrap="nowrap">
              <Text size="sm">{displayName}</Text>
              {hasRename && (
                <Badge size="xs" color="yellow" variant="light">
                  {column.name}
                </Badge>
              )}
              {column.unit && (
                <Badge size="xs" color="grape" variant="light">
                  {column.unit}
                </Badge>
              )}
            </Group>
          )}
        </Table.Td>
        <Table.Td style={{ whiteSpace: "nowrap", overflow: "visible" }}>
          <Group gap="xs" wrap="nowrap">
            <Badge size="xs" variant="light" style={{ flexShrink: 0 }}>
              {column.type}
            </Badge>
            {column.conversion?.selected && (
              <Badge
                size="xs"
                color="violet"
                variant="light"
                style={{ flexShrink: 0 }}
              >
                → {column.effectiveType}
              </Badge>
            )}
          </Group>
        </Table.Td>
        <Table.Td>
          {column.notes ? (
            <Text size="xs" c="dimmed" lineClamp={2}>
              {column.notes}
            </Text>
          ) : (
            <Text size="xs" c="dimmed" fs="italic">
              —
            </Text>
          )}
        </Table.Td>
        <Table.Td>
          <Group gap={4}>
            <Tooltip label="Rename">
              <ActionIcon
                size="sm"
                variant="light"
                color="blue"
                disabled={disabled}
                onClick={() => startEditingColumn(column)}
              >
                <IconEdit size={14} />
              </ActionIcon>
            </Tooltip>
            <Menu position="bottom-end" withinPortal>
              <Menu.Target>
                <ActionIcon size="sm" variant="subtle" disabled={disabled}>
                  <IconDotsVertical size={14} />
                </ActionIcon>
              </Menu.Target>
              <Menu.Dropdown>
                <Menu.Item
                  leftSection={<IconNotes size={14} />}
                  onClick={() => onColumnNotesClick?.(column)}
                >
                  Add Notes
                </Menu.Item>
                {isStringType && (
                  <Menu.Item
                    leftSection={<IconCashEdit size={14} />}
                    onClick={() => onColumnConversionClick?.(column)}
                  >
                    Configure Conversion
                  </Menu.Item>
                )}
              </Menu.Dropdown>
            </Menu>
          </Group>
        </Table.Td>
      </Table.Tr>
    );
  };

  const renderComputedColumnRow = (column: ComputedColumn) => {
    const isEditing = editingComputedColumnId === column.id;

    return (
      <Table.Tr key={column.id}>
        <Table.Td>
          <Checkbox
            disabled={disabled}
            checked={column.selected}
            onChange={(event) =>
              onComputedColumnSelectedChange?.(
                column.id,
                event.currentTarget.checked,
              )
            }
          />
        </Table.Td>
        <Table.Td>
          {isEditing ? (
            <Group gap="xs">
              <TextInput
                size="xs"
                value={editingComputedValue}
                onChange={(e) => setEditingComputedValue(e.currentTarget.value)}
                onKeyDown={(e) => {
                  if (e.key === "Enter") submitComputedColumnRename(column);
                  if (e.key === "Escape") cancelEditingComputedColumn();
                }}
                autoFocus
                style={{ flex: 1, minWidth: 150 }}
              />
              <ActionIcon
                size="sm"
                color="green"
                variant="light"
                onClick={() => submitComputedColumnRename(column)}
              >
                <IconCheck size={14} />
              </ActionIcon>
              <ActionIcon
                size="sm"
                color="gray"
                variant="light"
                onClick={cancelEditingComputedColumn}
              >
                <IconX size={14} />
              </ActionIcon>
            </Group>
          ) : (
            <Group gap="xs" wrap="nowrap">
              <Text size="sm">{column.name}</Text>
              <Badge size="xs" color="teal" variant="light">
                Computed
              </Badge>
              {column.unit && (
                <Badge size="xs" color="grape" variant="light">
                  {column.unit}
                </Badge>
              )}
            </Group>
          )}
        </Table.Td>
        <Table.Td style={{ whiteSpace: "nowrap", overflow: "visible" }}>
          <Badge size="xs" variant="light" style={{ flexShrink: 0 }}>
            {column.type}
          </Badge>
        </Table.Td>
        <Table.Td>
          {column.notes ? (
            <Text size="xs" c="dimmed" lineClamp={2}>
              {column.notes}
            </Text>
          ) : (
            <Text size="xs" c="dimmed" fs="italic">
              —
            </Text>
          )}
        </Table.Td>
        <Table.Td>
          <Group gap={4}>
            <Tooltip label="Rename">
              <ActionIcon
                size="sm"
                variant="light"
                color="blue"
                disabled={disabled}
                onClick={() => startEditingComputedColumn(column)}
              >
                <IconEdit size={14} />
              </ActionIcon>
            </Tooltip>
            <Menu position="bottom-end" withinPortal>
              <Menu.Target>
                <ActionIcon size="sm" variant="subtle" disabled={disabled}>
                  <IconDotsVertical size={14} />
                </ActionIcon>
              </Menu.Target>
              <Menu.Dropdown>
                <Menu.Item
                  leftSection={<IconNotes size={14} />}
                  onClick={() => onComputedColumnNotesClick?.(column)}
                >
                  Add Notes
                </Menu.Item>
                <Menu.Divider />
                <Menu.Item
                  leftSection={<IconTrash size={14} />}
                  color="red"
                  onClick={() => _onComputedColumnDelete?.(column.id)}
                >
                  Delete Column
                </Menu.Item>
              </Menu.Dropdown>
            </Menu>
          </Group>
        </Table.Td>
      </Table.Tr>
    );
  };

  const activeCount = activeColumns.length + activeComputedColumns.length;
  const inactiveCount = inactiveColumns.length + inactiveComputedColumns.length;

  return (
    <Box>
      <Group justify="space-between" mb="sm">
        <Badge color="teal" size="sm">
          Columns
        </Badge>
        <TextInput
          placeholder="Filter columns..."
          size="xs"
          leftSection={<IconSearch size={12} />}
          w={200}
          value={filterValue}
          onChange={(e) => setFilterValue(e.currentTarget.value)}
        />
      </Group>
      <Table
        striped
        highlightOnHover
        verticalSpacing={2}
        horizontalSpacing="xs"
      >
        <Table.Thead>
          <Table.Tr>
            <Table.Th w={40}>On</Table.Th>
            <Table.Th style={{ whiteSpace: "nowrap" }}>Name</Table.Th>
            <Table.Th style={{ whiteSpace: "nowrap", minWidth: 100 }}>
              Type
            </Table.Th>
            <Table.Th>Notes</Table.Th>
            <Table.Th w={80}>Actions</Table.Th>
          </Table.Tr>
        </Table.Thead>
        <Table.Tbody>
          {/* Active columns section */}
          {activeCount > 0 && (
            <>
              <Table.Tr
                style={{
                  cursor: "pointer",
                  backgroundColor: "var(--mantine-color-gray-1)",
                }}
                onClick={() => setActiveExpanded(!activeExpanded)}
              >
                <Table.Td colSpan={5}>
                  <Group justify="space-between">
                    <Group gap="sm">
                      {activeExpanded ? (
                        <IconChevronUp size={14} />
                      ) : (
                        <IconChevronDown size={14} />
                      )}
                      <Text fw={600} fz="sm">
                        Active Columns
                      </Text>
                      <Badge size="xs" variant="filled" color="teal">
                        {activeCount}
                      </Badge>
                    </Group>
                  </Group>
                </Table.Td>
              </Table.Tr>
              <Table.Tr style={{ display: "none" }}>
                <Table.Td colSpan={5}>
                  <Collapse in={activeExpanded}>
                    <div />
                  </Collapse>
                </Table.Td>
              </Table.Tr>
              {activeExpanded && (
                <>
                  {activeColumns.map(renderColumnRow)}
                  {activeComputedColumns.map(renderComputedColumnRow)}
                </>
              )}
            </>
          )}

          {/* Inactive columns section */}
          {inactiveCount > 0 && (
            <>
              <Table.Tr
                style={{
                  cursor: "pointer",
                  backgroundColor: "var(--mantine-color-gray-1)",
                }}
                onClick={() => setInactiveExpanded(!inactiveExpanded)}
              >
                <Table.Td colSpan={5}>
                  <Group justify="space-between">
                    <Group gap="sm">
                      {inactiveExpanded ? (
                        <IconChevronUp size={14} />
                      ) : (
                        <IconChevronDown size={14} />
                      )}
                      <Text fw={600} fz="sm" c="dimmed">
                        Inactive Columns
                      </Text>
                      <Badge size="xs" variant="light" color="gray">
                        {inactiveCount}
                      </Badge>
                    </Group>
                  </Group>
                </Table.Td>
              </Table.Tr>
              {inactiveExpanded && (
                <>
                  {inactiveColumns.map(renderColumnRow)}
                  {inactiveComputedColumns.map(renderComputedColumnRow)}
                </>
              )}
            </>
          )}
        </Table.Tbody>
      </Table>
    </Box>
  );
}
