"use client";

import { useState } from "react";
import { Modal, Stack, Select, Group, Button } from "@mantine/core";
import type { ContextField, TableInfo } from "./types";

export interface AddConditionModalProps {
  /** Whether the modal is open */
  opened: boolean;
  /** Callback when the modal is closed */
  onClose: () => void;
  /** Callback when a condition is submitted */
  onSubmit: (condition: {
    tableId: string;
    columnId: string;
    requestContextFieldId: string;
  }) => void;
  /** Available tables with their columns */
  tables: TableInfo[];
  /** Available context fields */
  fields: ContextField[];
  /** Whether the submit button should show loading state */
  loading?: boolean;
  /** Modal title (defaults to "Add Table Condition") */
  title?: string;
  /** Callback when a table is selected (for lazy loading columns) */
  onTableSelect?: (tableId: string) => void;
  /** Whether columns are currently loading */
  columnsLoading?: boolean;
}

export function AddConditionModal({
  opened,
  onClose,
  onSubmit,
  tables,
  fields,
  loading = false,
  title = "Add Table Condition",
  onTableSelect,
  columnsLoading = false,
}: AddConditionModalProps) {
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
        requestContextFieldId: fieldId,
      });
      // Reset form
      setTableId(null);
      setColumnId(null);
      setFieldId(null);
    }
  };

  const handleClose = () => {
    // Reset form on close
    setTableId(null);
    setColumnId(null);
    setFieldId(null);
    onClose();
  };

  const handleTableChange = (value: string | null) => {
    setTableId(value);
    setColumnId(null); // Reset column when table changes
    if (value && onTableSelect) {
      onTableSelect(value);
    }
  };

  return (
    <Modal opened={opened} onClose={handleClose} title={title}>
      <Stack gap="md">
        <Select
          label="Table"
          description="Select the table to apply row-level security"
          placeholder="Select a table"
          data={tables.map((t) => ({ value: t.id, label: t.name }))}
          value={tableId}
          onChange={handleTableChange}
        />
        <Select
          label="Context Field"
          description="Select the context field to match against"
          placeholder="Select a context field"
          data={fields.map((f) => ({ value: f.id, label: f.key }))}
          value={fieldId}
          onChange={setFieldId}
        />
        <Select
          label="Column"
          description="Select the column to filter on"
          placeholder={columnsLoading ? "Loading columns..." : "Select a column"}
          data={columns.map((c) => ({
            value: c.id,
            label: `${c.name} (${c.type})`,
          }))}
          value={columnId}
          onChange={setColumnId}
          disabled={!tableId || columnsLoading}
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
            Add Condition
          </Button>
        </Group>
      </Stack>
    </Modal>
  );
}
