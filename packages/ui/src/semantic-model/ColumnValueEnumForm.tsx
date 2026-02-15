"use client";

import {
  Alert,
  Box,
  Button,
  Group,
  Stack,
  Switch,
  Text,
  TextInput,
} from "@mantine/core";
import { IconInfoCircle, IconPlus, IconTrash } from "@tabler/icons-react";
import { useMemo, useState } from "react";
import { NUMERIC_LOGICAL_TYPES } from "@repo/types";
import type {
  Column,
  ColumnValueEnumCreatePayload,
  ColumnValueEnumEntryInput,
  ColumnValueEnumUpdatePayload,
} from "./types";

type EditableEnumEntry = {
  id: string;
  valueInput: string;
  label: string;
  selected: boolean;
};

function createEntry(overrides?: Partial<EditableEnumEntry>): EditableEnumEntry {
  return {
    id: Math.random().toString(36).slice(2),
    valueInput: "",
    label: "",
    selected: true,
    ...overrides,
  };
}

export interface ColumnValueEnumFormProps {
  column: Column;
  onCreate?: (payload: ColumnValueEnumCreatePayload) => Promise<void>;
  onUpdate?: (payload: ColumnValueEnumUpdatePayload) => Promise<void>;
  onDelete?: () => Promise<void>;
  onAutoFill?: () => Promise<ColumnValueEnumEntryInput[]>;
  onClose: () => void;
  loading?: boolean;
}

export function ColumnValueEnumForm({
  column,
  onCreate,
  onUpdate,
  onDelete,
  onAutoFill,
  onClose,
  loading = false,
}: ColumnValueEnumFormProps) {
  const existingEnum = column.valueEnum ?? null;
  const rawType = column.type.toLowerCase();
  const isNumeric = NUMERIC_LOGICAL_TYPES.has(rawType);
  const supportsEnums = isNumeric || rawType === "string";

  const initialEntries = useMemo(() => {
    if (!existingEnum || existingEnum.entries.length === 0) {
      return [createEntry()];
    }

    return existingEnum.entries
      .slice()
      .sort((a, b) => a.position - b.position)
      .map((entry) =>
        createEntry({
          valueInput: String(entry.value),
          label: entry.label,
          selected: entry.selected !== false,
        }),
      );
  }, [existingEnum]);

  const [enabled, setEnabled] = useState(existingEnum?.selected ?? true);
  const [entries, setEntries] = useState<EditableEnumEntry[]>(initialEntries);
  const [autoLoading, setAutoLoading] = useState(false);
  const [error, setError] = useState<string | null>(null);

  const updateEntry = (
    entryId: string,
    patch: Partial<EditableEnumEntry>,
  ): void => {
    setEntries((prev) =>
      prev.map((entry) => (entry.id === entryId ? { ...entry, ...patch } : entry)),
    );
  };

  const removeEntry = (entryId: string): void => {
    setEntries((prev) => prev.filter((entry) => entry.id !== entryId));
  };

  const normalizeEntries = () => {
    if (entries.length === 0) {
      throw new Error("Add at least one enum entry.");
    }

    const seenValueTokens = new Set<string>();
    const seenLabelTokens = new Set<string>();

    return entries.map((entry, index) => {
      const trimmedLabel = entry.label.trim();
      if (!trimmedLabel) {
        throw new Error(`Entry ${index + 1} is missing a label.`);
      }
      let value: string | number;
      if (isNumeric) {
        const parsed = Number(entry.valueInput.trim());
        if (!Number.isFinite(parsed)) {
          throw new Error(`Entry ${index + 1} must have a numeric value.`);
        }
        value = parsed;
      } else {
        const trimmedValue = entry.valueInput.trim();
        if (!trimmedValue) {
          throw new Error(`Entry ${index + 1} is missing a value.`);
        }
        value = trimmedValue;
      }

      const valueToken =
        typeof value === "number" ? `n:${value}` : `s:${value.toLowerCase()}`;
      if (seenValueTokens.has(valueToken)) {
        throw new Error(`Duplicate enum value "${String(value)}".`);
      }
      seenValueTokens.add(valueToken);

      const labelToken = `l:${trimmedLabel.toLowerCase()}`;
      if (seenLabelTokens.has(labelToken)) {
        throw new Error(`Duplicate enum label "${trimmedLabel}".`);
      }
      seenLabelTokens.add(labelToken);

      return {
        value,
        label: trimmedLabel,
        selected: entry.selected,
        position: index,
      };
    });
  };

  const handleSave = async () => {
    setError(null);
    if (!supportsEnums) {
      setError("Enums are only supported for string or numeric columns.");
      return;
    }

    try {
      const normalizedEntries = normalizeEntries();
      const payload: ColumnValueEnumCreatePayload = {
        selected: enabled,
        entries: normalizedEntries,
      };

      if (existingEnum && onUpdate) {
        await onUpdate(payload);
        return;
      }
      if (onCreate) {
        await onCreate(payload);
      }
    } catch (err) {
      setError(err instanceof Error ? err.message : "Failed to save enum.");
    }
  };

  const handleDelete = async () => {
    if (!existingEnum || !onDelete) return;
    setError(null);
    try {
      await onDelete();
    } catch (err) {
      setError(err instanceof Error ? err.message : "Failed to delete enum.");
    }
  };

  const handleAutoFill = async () => {
    if (!onAutoFill) return;
    setError(null);
    setAutoLoading(true);
    try {
      const autoEntries = await onAutoFill();
      if (autoEntries.length === 0) {
        setError("No distinct values were found for this column.");
        return;
      }
      setEntries(
        autoEntries.map((entry) =>
          createEntry({
            valueInput: String(entry.value),
            label: entry.label?.trim() || String(entry.value),
            selected: entry.selected ?? true,
          }),
        ),
      );
    } catch (err) {
      setError(
        err instanceof Error
          ? err.message
          : "Failed to auto-generate enum entries.",
      );
    } finally {
      setAutoLoading(false);
    }
  };

  if (!supportsEnums) {
    return (
      <Stack gap="sm">
        <Alert color="yellow" icon={<IconInfoCircle size={16} />}>
          Enum configuration is only available for string and numeric columns.
        </Alert>
        <Group justify="flex-end">
          <Button variant="default" onClick={onClose}>
            Close
          </Button>
        </Group>
      </Stack>
    );
  }

  return (
    <Stack gap="sm">
      <Alert color="blue" icon={<IconInfoCircle size={16} />}>
        Map DB values to semantic labels for the agent. The agent will be able
        to read the labels when describing the schema and use them when
        constructing queries.
      </Alert>

      <Switch
        label="Enable enum"
        checked={enabled}
        onChange={(event) => setEnabled(event.currentTarget.checked)}
        disabled={loading}
      />

      <Stack gap="xs">
        <Group justify="space-between">
          <Text size="sm" fw={500}>
            Entries
          </Text>
          <Group gap="xs">
            {onAutoFill && (
              <Button
                size="xs"
                variant="default"
                onClick={() => void handleAutoFill()}
                loading={autoLoading}
                disabled={loading}
              >
                Auto-fill distinct values
              </Button>
            )}
            <Button
              size="xs"
              variant="light"
              leftSection={<IconPlus size={14} />}
              onClick={() => setEntries((prev) => [...prev, createEntry()])}
              disabled={loading}
            >
              Add entry
            </Button>
          </Group>
        </Group>
        {entries.map((entry, index) => (
          <Box
            key={entry.id}
            p="sm"
            style={{
              border: "1px solid var(--mantine-color-gray-3)",
              borderRadius: 8,
              backgroundColor: "var(--mantine-color-gray-0)",
            }}
          >
            <Stack gap="xs">
              <Group justify="space-between">
                <Text size="xs" c="dimmed">
                  Entry {index + 1}
                </Text>
                <Button
                  size="compact-xs"
                  variant="subtle"
                  color="red"
                  leftSection={<IconTrash size={12} />}
                  onClick={() => removeEntry(entry.id)}
                  disabled={loading || entries.length <= 1}
                >
                  Remove
                </Button>
              </Group>

              <TextInput
                label={isNumeric ? "Value (number)" : "Value"}
                description="Canonical value stored in DB and used in final filters."
                placeholder={isNumeric ? "e.g. 1" : "e.g. joined"}
                value={entry.valueInput}
                onChange={(event) =>
                  updateEntry(entry.id, { valueInput: event.currentTarget.value })
                }
                disabled={loading}
              />
              <TextInput
                label="Label"
                description="Required semantic label the agent can use."
                placeholder="e.g. Joined"
                value={entry.label}
                onChange={(event) =>
                  updateEntry(entry.id, { label: event.currentTarget.value })
                }
                disabled={loading}
              />
              <Switch
                label="Entry enabled"
                checked={entry.selected}
                onChange={(event) =>
                  updateEntry(entry.id, { selected: event.currentTarget.checked })
                }
                disabled={loading}
              />
            </Stack>
          </Box>
        ))}
      </Stack>

      {error && (
        <Alert color="red" icon={<IconInfoCircle size={16} />}>
          {error}
        </Alert>
      )}

      <Group justify="space-between" mt="xs">
        <Group>
          {existingEnum && onDelete && (
            <Button
              color="red"
              variant="light"
              onClick={handleDelete}
              loading={loading}
            >
              Delete enum
            </Button>
          )}
        </Group>
        <Group>
          <Button variant="default" onClick={onClose} disabled={loading}>
            Cancel
          </Button>
          <Button onClick={() => void handleSave()} loading={loading}>
            Save enum
          </Button>
        </Group>
      </Group>
    </Stack>
  );
}
