import { useState } from "react";
import {
  Select,
  TextInput,
  Button,
  Group,
  Stack,
  Text,
  Alert,
} from "@mantine/core";
import { useForm } from "@mantine/form";
import { IconInfoCircle } from "@tabler/icons-react";
import type { TableSchema, ColumnUnitPayload } from "./types";

export interface UnitsFormProps {
  /** The table schema (used to get available columns) */
  table: Pick<TableSchema, "id" | "name" | "columns" | "computedColumns">;
  /** Callback when a unit is added */
  onSave: (payload: ColumnUnitPayload) => Promise<void>;
  /** Callback when cancel/close is clicked */
  onClose: () => void;
  /** Whether a save is in progress */
  loading?: boolean;
}

export function UnitsForm({
  table,
  onSave,
  onClose,
  loading = false,
}: UnitsFormProps) {
  const [selectedColumnId, setSelectedColumnId] = useState<string>("");

  const form = useForm<{ unit: string }>({
    initialValues: {
      unit: "",
    },
    validate: {
      unit: (value: string) => {
        if (value.length > 50) {
          return "Unit must be less than 50 characters";
        }
        return null;
      },
    },
  });

  // Get numeric columns without existing units
  const numericColumns = [
    ...(table.columns ?? []).filter((col) =>
      ["number"].some((type) => col.type.toLowerCase().includes(type))
    ),
    ...(table.computedColumns ?? []).filter((col) =>
      ["number"].some((type) => col.type.toLowerCase().includes(type))
    ),
  ];

  const columnOptions = numericColumns
    .filter((col) => !col.unit)
    .map((col) => ({
      value: col.id,
      label: col.name,
    }));

  const handleSubmit = async () => {
    const validation = form.validate();
    if (validation.hasErrors || !selectedColumnId) return;

    const selectedColumn = numericColumns.find(
      (col) => col.id === selectedColumnId
    );
    if (!selectedColumn) return;

    await onSave({
      columnName: selectedColumn.name,
      unit: form.values.unit.trim(),
    });

    form.reset();
    setSelectedColumnId("");
  };

  return (
    <Stack>
      <Alert
        icon={<IconInfoCircle size={16} />}
        title="Configure Units"
        color="blue"
        variant="light"
      >
        <Text size="sm">
          Add units to numeric columns to provide context for data values (e.g.,
          &quot;USD&quot;, &quot;kg&quot;, &quot;miles&quot;).
        </Text>
      </Alert>

      <Select
        label="Select Column"
        placeholder="Choose a numeric column"
        value={selectedColumnId}
        onChange={(value) => setSelectedColumnId(value ?? "")}
        data={columnOptions}
        description="Only numeric columns without units are shown"
        disabled={columnOptions.length === 0}
      />

      {columnOptions.length === 0 && (
        <Alert color="gray" variant="light">
          <Text size="sm">
            All numeric columns already have units configured.
          </Text>
        </Alert>
      )}

      <TextInput
        label="Unit"
        placeholder="e.g., USD, kg, miles, %"
        {...form.getInputProps("unit")}
        disabled={!selectedColumnId}
      />

      <Group justify="flex-end" mt="md">
        <Button variant="outline" onClick={onClose} disabled={loading}>
          Cancel
        </Button>
        <Button
          onClick={handleSubmit}
          loading={loading}
          disabled={!selectedColumnId}
        >
          Add Unit
        </Button>
      </Group>
    </Stack>
  );
}
