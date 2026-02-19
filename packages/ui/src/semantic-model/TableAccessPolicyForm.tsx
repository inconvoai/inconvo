import { useState } from "react";
import { Select, Button, Group, Stack, Text, Alert } from "@mantine/core";
import { IconInfoCircle } from "@tabler/icons-react";
import type {
  TableSchema,
  UserContextField,
  TableAccessPolicyPayload,
} from "./types";

export interface TableAccessPolicyFormProps {
  /** The table being configured */
  table: Pick<TableSchema, "id" | "name" | "accessPolicy">;
  /** Available user context fields */
  userContextFields: UserContextField[];
  /** Callback when save is clicked */
  onSave: (payload: TableAccessPolicyPayload) => Promise<void>;
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

export function TableAccessPolicyForm({
  table,
  userContextFields,
  onSave,
  onDelete,
  onClose,
  saveLoading = false,
  deleteLoading = false,
  readOnly = false,
}: TableAccessPolicyFormProps) {
  const booleanFields = userContextFields.filter(
    (field) => field.type === "BOOLEAN",
  );

  const [fieldKey, setFieldKey] = useState(
    table.accessPolicy?.userContextField.key ?? "",
  );

  const handleSave = async () => {
    if (!fieldKey) return;
    const field = booleanFields.find((f) => f.key === fieldKey);
    if (!field) return;
    await onSave({ userContextFieldId: field.id });
  };

  const fieldOptions = booleanFields.map((field) => ({
    value: field.key,
    label: field.key,
  }));
  const hasBooleanFields = booleanFields.length > 0;
  const selectedField = booleanFields.find((field) => field.key === fieldKey);

  const isLoading = saveLoading || deleteLoading;
  const isDisabled = readOnly || isLoading;

  return (
    <Stack h="100%" justify="space-between">
      <Stack style={{ flex: 1, paddingBottom: "60px" }}>
        <Alert
          icon={<IconInfoCircle size={16} />}
          title="Table-level access policy"
          color="blue"
          variant="light"
        >
          <Text size="sm">
            This table is available only when the selected user context key has
            value <strong>true</strong> for the request.
          </Text>
        </Alert>

        <Select
          label="User Context Field"
          placeholder={
            hasBooleanFields ? "Select a boolean field" : "No boolean fields"
          }
          value={fieldKey}
          onChange={(value) => setFieldKey(value ?? "")}
          data={fieldOptions}
          description="Table is available when userContext.<key> == true"
          disabled={isDisabled || !hasBooleanFields}
        />

        {!hasBooleanFields && (
          <Alert color="yellow" variant="light">
            <Text size="sm">
              Add at least one <strong>BOOLEAN</strong> user context field to
              configure a table access policy.
            </Text>
          </Alert>
        )}

        {table.accessPolicy && (
          <Alert color="gray" variant="light">
            <Text size="sm">
              Current policy: Available when{" "}
              <strong>userContext.{table.accessPolicy.userContextField.key}</strong>{" "}
              equals <strong>true</strong>
            </Text>
          </Alert>
        )}

        <Group justify="flex-end" grow>
          <Button
            variant="outline"
            color="red"
            onClick={onDelete}
            loading={deleteLoading}
            disabled={readOnly || !table.accessPolicy || saveLoading}
          >
            Delete
          </Button>
          <Button variant="outline" onClick={onClose} disabled={isLoading}>
            Cancel
          </Button>
          <Button
            onClick={handleSave}
            loading={saveLoading}
            disabled={readOnly || !selectedField || deleteLoading}
          >
            Save
          </Button>
        </Group>
      </Stack>
    </Stack>
  );
}
