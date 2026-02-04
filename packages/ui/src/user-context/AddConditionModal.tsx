"use client";

import { useState } from "react";
import { Drawer, Stack, Select, Group, Button } from "@mantine/core";
import type { ContextField, TableInfo, ConnectionInfo } from "./types";

export interface AddConditionModalProps {
  /** Whether the drawer is open */
  opened: boolean;
  /** Callback when the drawer is closed */
  onClose: () => void;
  /** Callback when a condition is submitted */
  onSubmit: (condition: {
    tableId: string;
    columnId: string;
    userContextFieldId: string;
  }) => void;
  /** Available connections/databases */
  connections: ConnectionInfo[];
  /** Available tables with their columns (filtered by selected connection) */
  tables: TableInfo[];
  /** Available context fields */
  fields: ContextField[];
  /** Whether the submit button should show loading state */
  loading?: boolean;
  /** Drawer title (defaults to "Add Access Constraint") */
  title?: string;
  /** Callback when a connection is selected (for loading tables) */
  onConnectionSelect?: (connectionId: string) => void;
  /** Whether tables are currently loading */
  tablesLoading?: boolean;
  /** Callback when a table is selected (for lazy loading columns) */
  onTableSelect?: (tableId: string) => void;
  /** Whether columns are currently loading */
  columnsLoading?: boolean;
}

export function AddConditionModal({
  opened,
  onClose,
  onSubmit,
  connections,
  tables,
  fields,
  loading = false,
  title = "Add Access Constraint",
  onConnectionSelect,
  tablesLoading = false,
  onTableSelect,
  columnsLoading = false,
}: AddConditionModalProps) {
  const [connectionId, setConnectionId] = useState<string | null>(null);
  const [tableId, setTableId] = useState<string | null>(null);
  const [columnId, setColumnId] = useState<string | null>(null);
  const [fieldId, setFieldId] = useState<string | null>(null);

  const selectedTable = tables.find((t) => t.id === tableId);
  const columns = selectedTable?.columns ?? [];

  const handleSubmit = () => {
    if (tableId && columnId && fieldId) {
      onSubmit({
        tableId,
        columnId,
        userContextFieldId: fieldId,
      });
      // Reset form
      setConnectionId(null);
      setTableId(null);
      setColumnId(null);
      setFieldId(null);
    }
  };

  const handleClose = () => {
    // Reset form on close
    setConnectionId(null);
    setTableId(null);
    setColumnId(null);
    setFieldId(null);
    onClose();
  };

  const handleConnectionChange = (value: string | null) => {
    setConnectionId(value);
    setTableId(null); // Reset table when connection changes
    setColumnId(null); // Reset column when connection changes
    if (value && onConnectionSelect) {
      onConnectionSelect(value);
    }
  };

  const handleTableChange = (value: string | null) => {
    setTableId(value);
    setColumnId(null); // Reset column when table changes
    if (value && onTableSelect) {
      onTableSelect(value);
    }
  };

  return (
    <Drawer
      opened={opened}
      onClose={handleClose}
      title={title}
      position="right"
      size="md"
    >
      <Stack gap="md">
        <Select
          label="Database"
          description="Select the database connection"
          placeholder="Select a database"
          data={connections.map((c) => ({ value: c.id, label: c.name }))}
          value={connectionId}
          onChange={handleConnectionChange}
        />
        <Select
          label="Table"
          description="Select the table to apply an access constraint"
          placeholder={tablesLoading ? "Loading tables..." : "Select a table"}
          data={tables.map((t) => ({ value: t.id, label: t.name }))}
          value={tableId}
          onChange={handleTableChange}
          disabled={!connectionId || tablesLoading}
        />
        <Select
          label="Column"
          description="Select the column to filter on"
          placeholder={
            columnsLoading ? "Loading columns..." : "Select a column"
          }
          data={columns.map((c) => ({
            value: c.id,
            label: `${c.name} (${c.type})`,
          }))}
          value={columnId}
          onChange={setColumnId}
          disabled={!tableId || columnsLoading}
        />
        <Select
          label="Context Field"
          description="Select the context field to match against"
          placeholder="Select a context field"
          data={fields.map((f) => ({ value: f.id, label: f.key }))}
          value={fieldId}
          onChange={setFieldId}
        />
        <Group justify="flex-end" gap="sm">
          <Button variant="subtle" onClick={handleClose}>
            Cancel
          </Button>
          <Button
            onClick={handleSubmit}
            disabled={!tableId || !columnId || !fieldId}
            loading={loading}
          >
            Add Constraint
          </Button>
        </Group>
      </Stack>
    </Drawer>
  );
}
