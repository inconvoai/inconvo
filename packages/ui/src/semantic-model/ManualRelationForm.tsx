"use client";

import {
  Alert,
  Box,
  Button,
  Divider,
  Group,
  Select,
  Stack,
  Switch,
  Text,
  TextInput,
  ActionIcon,
} from "@mantine/core";
import { useForm } from "@mantine/form";
import { IconAlertCircle, IconPlus, IconTrash } from "@tabler/icons-react";
import { useEffect, useMemo, useState } from "react";
import type {
  TableSchema,
  Relation,
  TableWithColumns,
  ManualRelationCreatePayload,
  ManualRelationUpdatePayload,
} from "./types";

type ColumnPair = {
  sourceColumnName: string;
  targetColumnName: string;
};

interface ManualRelationFormValues {
  name: string;
  isList: boolean;
  targetTableId: string;
  columnPairs: ColumnPair[];
}

export interface ManualRelationFormProps {
  /** The source table schema */
  table: Pick<TableSchema, "id" | "name" | "columns">;
  /** Available tables to select as targets */
  availableTables: TableWithColumns[];
  /** Existing relation to edit (if editing) */
  relation?: Relation;
  /** Callback when a new relation is created */
  onCreate?: (payload: ManualRelationCreatePayload) => Promise<void>;
  /** Callback when an existing relation is updated */
  onUpdate?: (
    relationId: string,
    payload: ManualRelationUpdatePayload,
  ) => Promise<void>;
  /** Callback when cancel/close is clicked */
  onClose: () => void;
  /** Whether a save is in progress */
  loading?: boolean;
}

const NAME_PATTERN = /^[a-z_]+$/i;

function validateRelationName(value: string) {
  if (!value || value.trim().length === 0) {
    return "Name is required";
  }
  if (!NAME_PATTERN.test(value)) {
    return "Use only letters and underscores (no spaces or numbers)";
  }
  return null;
}

export function ManualRelationForm({
  table,
  availableTables,
  relation,
  onCreate,
  onUpdate,
  onClose,
  loading = false,
}: ManualRelationFormProps) {
  const isEditing = Boolean(relation);
  const [submitError, setSubmitError] = useState<string | null>(null);

  const form = useForm<ManualRelationFormValues>({
    mode: "uncontrolled",
    initialValues: {
      name: relation?.name ?? "",
      isList: relation?.isList ?? false,
      targetTableId: relation?.targetTableId ?? "",
      columnPairs: relation?.columnMappings.map((mapping) => ({
        sourceColumnName: mapping.sourceColumnName,
        targetColumnName: mapping.targetColumnName,
      })) ?? [
        {
          sourceColumnName: "",
          targetColumnName: "",
        },
      ],
    },
  });

  // Set default target table when creating new relation
  useEffect(() => {
    if (!isEditing && !form.values.targetTableId) {
      const defaultTarget = availableTables.find((t) => t.id !== table.id);
      if (defaultTarget?.id) {
        form.setFieldValue("targetTableId", defaultTarget.id);
      }
    }
  }, [isEditing, form, availableTables, table.id]);

  const sourceColumnOptions = useMemo(
    () =>
      table.columns.map((column) => ({
        label: column.name,
        value: column.name,
      })),
    [table.columns],
  );

  const targetTableOptions = useMemo(
    () =>
      availableTables
        .filter((t) => t.id !== table.id)
        .map((t) => ({
          label: t.name,
          value: t.id,
        })),
    [availableTables, table.id],
  );

  const selectedTargetTable = useMemo(() => {
    return availableTables.find((t) => t.id === form.values.targetTableId);
  }, [form.values.targetTableId, availableTables]);

  const targetColumnOptions = useMemo(
    () =>
      selectedTargetTable?.columns.map((column) => ({
        label: column.name,
        value: column.name,
      })) ?? [],
    [selectedTargetTable],
  );

  const handleSubmit = form.onSubmit(
    async (values: ManualRelationFormValues) => {
      setSubmitError(null);

      if (!values.targetTableId) {
        setSubmitError("Please select a target table.");
        return;
      }
      if (
        values.columnPairs.length === 0 ||
        values.columnPairs.some(
          (pair: ColumnPair) =>
            !pair.sourceColumnName || !pair.targetColumnName,
        )
      ) {
        setSubmitError("All column mappings must have both source and target.");
        return;
      }

      const payload = {
        name: values.name.trim(),
        isList: values.isList,
        targetTableId: values.targetTableId,
        columnPairs: values.columnPairs,
      };

      try {
        if (isEditing && relation?.id && onUpdate) {
          await onUpdate(relation.id, payload);
        } else if (onCreate) {
          await onCreate(payload);
        }
      } catch (error) {
        setSubmitError(
          error instanceof Error
            ? error.message
            : isEditing
              ? "Failed to update manual relation."
              : "Failed to create manual relation.",
        );
      }
    },
  );

  const addColumnPair = () => {
    form.insertListItem("columnPairs", {
      sourceColumnName: "",
      targetColumnName: "",
    });
  };

  const removeColumnPair = (index: number) => {
    if (form.values.columnPairs.length === 1) {
      form.setFieldValue("columnPairs", [
        { sourceColumnName: "", targetColumnName: "" },
      ]);
      return;
    }
    form.removeListItem("columnPairs", index);
  };

  return (
    <Stack gap="md">
      {submitError && (
        <Alert
          color="red"
          icon={<IconAlertCircle size={16} />}
          title="Something went wrong"
        >
          {submitError}
        </Alert>
      )}

      {relation?.status === "BROKEN" && (
        <Alert
          color="yellow"
          icon={<IconAlertCircle size={16} />}
          title="Relation needs attention"
        >
          {relation.errorTag ??
            "One or more columns referenced by this relation are missing."}
        </Alert>
      )}

      <form onSubmit={handleSubmit}>
        <Stack gap="sm">
          <TextInput
            label="Relation Name"
            placeholder="source_to_target"
            required
            withAsterisk
            description="Only letters and underscores (e.g. source_target)"
            disabled={loading}
            {...form.getInputProps("name", { validate: validateRelationName })}
          />

          <Select
            label="Target Table"
            placeholder="Select a table"
            data={targetTableOptions}
            searchable
            required
            nothingFoundMessage="No tables"
            disabled={loading}
            {...form.getInputProps("targetTableId")}
          />

          <Group justify="space-between">
            <Text fw={500}>Relation Type</Text>
            <Switch
              label="One-to-many (list)"
              checked={form.values.isList}
              onChange={(event) =>
                form.setFieldValue("isList", event.currentTarget.checked)
              }
              disabled={loading}
            />
          </Group>

          <Divider label="Column Mappings" labelPosition="center" />

          <Stack gap="xs">
            {form.values.columnPairs.map((pair: ColumnPair, index: number) => (
              <Box
                key={`mapping-${index}`}
                style={{
                  border: "1px solid var(--mantine-color-gray-3)",
                  borderRadius: "var(--mantine-radius-sm)",
                  padding: "var(--mantine-spacing-xs)",
                }}
              >
                <Group justify="space-between" mb="xs">
                  <Text fw={500}>Mapping {index + 1}</Text>
                  <ActionIcon
                    variant="subtle"
                    color="red"
                    aria-label="Remove mapping"
                    onClick={() => removeColumnPair(index)}
                    disabled={loading}
                  >
                    <IconTrash size={16} />
                  </ActionIcon>
                </Group>
                <Group align="flex-end">
                  <Select
                    label="Source column"
                    placeholder="source_table.foreign_key"
                    data={sourceColumnOptions}
                    searchable
                    required
                    flex={1}
                    value={pair.sourceColumnName}
                    disabled={loading}
                    onChange={(value) =>
                      form.setFieldValue(
                        "columnPairs",
                        form.values.columnPairs.map(
                          (existing: ColumnPair, idx: number) =>
                            idx === index
                              ? {
                                  ...existing,
                                  sourceColumnName: value ?? "",
                                }
                              : existing,
                        ),
                      )
                    }
                  />
                  <Select
                    label="Target column"
                    placeholder="target_table.primary_key"
                    data={targetColumnOptions}
                    searchable
                    required
                    flex={1}
                    value={pair.targetColumnName}
                    disabled={loading}
                    onChange={(value) =>
                      form.setFieldValue(
                        "columnPairs",
                        form.values.columnPairs.map(
                          (existing: ColumnPair, idx: number) =>
                            idx === index
                              ? {
                                  ...existing,
                                  targetColumnName: value ?? "",
                                }
                              : existing,
                        ),
                      )
                    }
                  />
                </Group>
              </Box>
            ))}
          </Stack>

          <Button
            variant="light"
            leftSection={<IconPlus size={16} />}
            onClick={addColumnPair}
            disabled={loading}
          >
            Add Column Mapping
          </Button>

          <Divider />

          <Group justify="flex-end">
            <Button variant="outline" onClick={onClose} disabled={loading}>
              Cancel
            </Button>
            <Button type="submit" loading={loading}>
              {isEditing ? "Save Changes" : "Create Relation"}
            </Button>
          </Group>
        </Stack>
      </form>
    </Stack>
  );
}
