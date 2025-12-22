import { useState } from "react";
import { Select, Button, Group, Stack, Text, Alert } from "@mantine/core";
import { IconInfoCircle } from "@tabler/icons-react";
import type {
  TableSchema,
  RequestContextField,
  ContextFilterPayload,
} from "./types";

export interface ContextFilterFormProps {
  /** The table being configured */
  table: Pick<TableSchema, "id" | "name" | "columns" | "condition">;
  /** Available request context fields */
  requestContextFields: RequestContextField[];
  /** Callback when save is clicked */
  onSave: (payload: ContextFilterPayload) => Promise<void>;
  /** Callback when delete is clicked */
  onDelete: () => Promise<void>;
  /** Callback when cancel/close is clicked */
  onClose: () => void;
  /** Whether a save operation is in progress */
  saveLoading?: boolean;
  /** Whether a delete operation is in progress */
  deleteLoading?: boolean;
}

export function ContextFilterForm({
  table,
  requestContextFields,
  onSave,
  onDelete,
  onClose,
  saveLoading = false,
  deleteLoading = false,
}: ContextFilterFormProps) {
  const [columnName, setColumnName] = useState(
    table.condition?.column.name ?? ""
  );
  const [fieldKey, setFieldKey] = useState(
    table.condition?.requestContextField.key ?? ""
  );

  const handleSave = async () => {
    if (!columnName || !fieldKey) return;

    const column = table.columns.find((col) => col.name === columnName);
    const field = requestContextFields.find((f) => f.key === fieldKey);

    if (column && field) {
      await onSave({
        columnId: column.id,
        requestContextFieldId: field.id,
      });
    }
  };

  const handleDelete = async () => {
    await onDelete();
  };

  const columnOptions = table.columns.map((column) => ({
    value: column.name,
    label: column.name,
  }));

  const requestContextOptions = requestContextFields.map((field) => ({
    value: field.key,
    label: field.key,
  }));

  const isLoading = saveLoading || deleteLoading;

  return (
    <Stack h="100%" justify="space-between">
      <Stack style={{ flex: 1, paddingBottom: "60px" }}>
        <Alert
          icon={<IconInfoCircle size={16} />}
          title="Row-level Security"
          color="blue"
          variant="light"
        >
          <Text size="sm">
            Where conditions filter table data based on the current user&apos;s
            context. Only rows matching the condition will be accessible.
          </Text>
        </Alert>

        <Select
          label="Column"
          placeholder="Select a column"
          value={columnName}
          onChange={(value) => setColumnName(value ?? "")}
          data={columnOptions}
          description="The column to filter on"
          disabled={isLoading}
        />

        <Select
          label="Request Context Field"
          placeholder="Select a field"
          value={fieldKey}
          onChange={(value) => setFieldKey(value ?? "")}
          data={requestContextOptions}
          description="The user context value to match against"
          disabled={isLoading}
        />

        {table.condition && (
          <Alert color="gray" variant="light">
            <Text size="sm">
              Current condition: Where{" "}
              <strong>
                {table.name}.{table.condition.column.name}
              </strong>{" "}
              equals{" "}
              <strong>
                requestContext.{table.condition.requestContextField.key}
              </strong>
            </Text>
          </Alert>
        )}

        <Group justify="flex-end" grow>
          <Button
            variant="outline"
            color="red"
            onClick={handleDelete}
            loading={deleteLoading}
            disabled={!table.condition || saveLoading}
          >
            Delete
          </Button>
          <Button variant="outline" onClick={onClose} disabled={isLoading}>
            Cancel
          </Button>
          <Button
            onClick={handleSave}
            loading={saveLoading}
            disabled={!columnName || !fieldKey || deleteLoading}
          >
            Save
          </Button>
        </Group>
      </Stack>
    </Stack>
  );
}
