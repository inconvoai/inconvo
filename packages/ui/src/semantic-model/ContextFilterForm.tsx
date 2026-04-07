import { useState } from "react";
import { Select, Button, Group, Stack, Text, Alert } from "@mantine/core";
import { IconInfoCircle } from "@tabler/icons-react";
import { doesUserContextValueTypeMatchColumnType } from "@repo/types";
import type {
  TableSchema,
  UserContextField,
  ContextFilterPayload,
} from "./types";

export interface ContextFilterFormProps {
  /** The table being configured */
  table: Pick<TableSchema, "id" | "name" | "columns" | "condition">;
  /** Available user context fields */
  userContextFields: UserContextField[];
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
  /** Whether the form is in read-only mode (disables all mutations) */
  readOnly?: boolean;
}

export function ContextFilterForm({
  table,
  userContextFields,
  onSave,
  onDelete,
  onClose,
  saveLoading = false,
  deleteLoading = false,
  readOnly = false,
}: ContextFilterFormProps) {
  const [columnName, setColumnName] = useState(
    table.condition?.column.name ?? "",
  );
  const [fieldKey, setFieldKey] = useState(
    table.condition?.userContextField.key ?? "",
  );

  const handleSave = async () => {
    if (!columnName || !fieldKey) return;

    const column = table.columns.find((col) => col.name === columnName);
    const field = compatibleUserContextFields.find((f) => f.key === fieldKey);

    if (column && field) {
      await onSave({
        columnId: column.id,
        userContextFieldId: field.id,
      });
    }
  };

  const handleDelete = async () => {
    await onDelete();
  };

  const selectedColumn = table.columns.find((col) => col.name === columnName);
  const selectedColumnType =
    selectedColumn?.effectiveType ?? selectedColumn?.type ?? null;
  const compatibleUserContextFields = selectedColumnType
    ? userContextFields.filter((field) =>
        doesUserContextValueTypeMatchColumnType(field.type, selectedColumnType),
      )
    : userContextFields;

  const handleColumnChange = (value: string | null) => {
    const nextColumnName = value ?? "";
    setColumnName(nextColumnName);

    const nextColumn = table.columns.find((col) => col.name === nextColumnName);
    const nextColumnType = nextColumn?.effectiveType ?? nextColumn?.type ?? null;
    if (!nextColumnType) {
      return;
    }

    const nextCompatibleFields = userContextFields.filter((field) =>
      doesUserContextValueTypeMatchColumnType(field.type, nextColumnType),
    );
    const currentFieldIsCompatible = nextCompatibleFields.some(
      (field) => field.key === fieldKey,
    );
    if (!currentFieldIsCompatible) {
      setFieldKey("");
    }
  };

  const columnOptions = table.columns.map((column) => ({
    value: column.name,
    label: column.name,
  }));

  const userContextOptions = compatibleUserContextFields.map((field) => ({
    value: field.key,
    label: field.key,
  }));
  const selectedField = compatibleUserContextFields.find(
    (field) => field.key === fieldKey,
  );

  const isLoading = saveLoading || deleteLoading;
  const isDisabled = readOnly || isLoading;

  return (
    <Stack h="100%" justify="space-between">
      <Stack style={{ flex: 1, paddingBottom: "60px" }}>
        <Alert
          icon={<IconInfoCircle size={16} />}
          title="Row-level access constraint"
          color="blue"
          variant="light"
        >
          <Text size="sm">
            Where conditions filter table data based on the current user&apos;s
            context. Only rows matching the condition will be accessible when
            user context is enabled.
          </Text>
        </Alert>

        <Select
          label="Column"
          placeholder="Select a column"
          value={columnName}
          onChange={handleColumnChange}
          data={columnOptions}
          description="The column to filter on"
          disabled={isDisabled}
        />

        <Select
          label="User Context Field"
          placeholder={
            selectedColumnType
              ? "Select a compatible field"
              : "Select a column first"
          }
          value={fieldKey}
          onChange={(value) => setFieldKey(value ?? "")}
          data={userContextOptions}
          description={
            selectedColumnType
              ? "Only compatible user context fields are shown for the selected column"
              : "Select a column to filter the available user context fields"
          }
          disabled={isDisabled || !selectedColumnType}
          nothingFoundMessage="No compatible user context fields found"
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
                userContext.{table.condition.userContextField.key}
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
            disabled={readOnly || !table.condition || saveLoading}
          >
            Delete
          </Button>
          <Button variant="outline" onClick={onClose} disabled={isLoading}>
            Cancel
          </Button>
          <Button
            onClick={handleSave}
            loading={saveLoading}
            disabled={
              readOnly || !columnName || !selectedField || deleteLoading
            }
          >
            Save
          </Button>
        </Group>
      </Stack>
    </Stack>
  );
}
