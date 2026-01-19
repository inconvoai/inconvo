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
import type {
  TableSchema,
  ColumnUnitPayload,
  ComputedColumnUnitPayload,
} from "./types";

export interface UnitsFormProps {
  /** The table schema (used to get available columns) */
  table: Pick<TableSchema, "id" | "name" | "columns" | "computedColumns">;
  /** Callback when a unit is added for a regular column */
  onSave: (payload: ColumnUnitPayload) => Promise<void>;
  /** Callback when a unit is added for a computed column */
  onSaveComputedColumn?: (payload: ComputedColumnUnitPayload) => Promise<void>;
  /** Callback when cancel/close is clicked */
  onClose: () => void;
  /** Whether a save is in progress */
  loading?: boolean;
}

export function UnitsForm({
  table,
  onSave,
  onSaveComputedColumn,
  onClose,
  loading = false,
}: UnitsFormProps) {
  const [selectedColumnId, setSelectedColumnId] = useState<string>("");

  // Derive isComputedColumn from selectedColumnId
  const isComputedColumn = selectedColumnId.startsWith("computed:");

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

  // Get numeric columns (regular columns)
  const numericColumns = (table.columns ?? []).filter((col) =>
    ["number"].some((type) => col.type.toLowerCase().includes(type)),
  );

  // Get numeric computed columns
  const numericComputedColumns = (table.computedColumns ?? []).filter((col) =>
    ["number"].some((type) => col.type.toLowerCase().includes(type)),
  );

  // Build column options with group info
  const columnOptions = [
    ...numericColumns.map((col) => ({
      value: col.id,
      label: col.unit ? `${col.name} (${col.unit})` : col.name,
    })),
    ...numericComputedColumns.map((col) => ({
      value: `computed:${col.id}`,
      label: col.unit
        ? `${col.name} (${col.unit}) [computed]`
        : `${col.name} [computed]`,
    })),
  ];

  const handleSubmit = async () => {
    const validation = form.validate();
    if (validation.hasErrors || !selectedColumnId) return;

    const trimmedUnit = form.values.unit.trim();

    if (isComputedColumn) {
      // Handle computed column
      const computedColumnId = selectedColumnId.replace("computed:", "");
      if (onSaveComputedColumn) {
        await onSaveComputedColumn({
          computedColumnId,
          unit: trimmedUnit || null,
        });
      }
    } else {
      // Handle regular column
      const selectedColumn = numericColumns.find(
        (col) => col.id === selectedColumnId,
      );
      if (!selectedColumn) return;

      await onSave({
        columnName: selectedColumn.name,
        unit: trimmedUnit,
      });
    }

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
        onChange={(value) => {
          setSelectedColumnId(value ?? "");

          if (value?.startsWith("computed:")) {
            const computedColumnId = value.replace("computed:", "");
            const computedColumn = numericComputedColumns.find(
              (col) => col.id === computedColumnId,
            );
            form.setFieldValue("unit", computedColumn?.unit ?? "");
          } else {
            const column = numericColumns.find((col) => col.id === value);
            form.setFieldValue("unit", column?.unit ?? "");
          }
        }}
        data={columnOptions}
        description="Select a numeric column to add or edit its unit"
        disabled={columnOptions.length === 0}
      />

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
          Save Unit
        </Button>
      </Group>
    </Stack>
  );
}
