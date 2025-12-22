import { useState } from "react";
import { Textarea, Button, Group, Stack, Text, Badge } from "@mantine/core";
import type { Column } from "./types";

export interface ColumnNotesFormProps {
  /** The column being edited */
  column: Pick<Column, "id" | "name" | "rename" | "notes">;
  /** The name of the table containing the column */
  tableName: string;
  /** Callback when save is clicked */
  onSave: (notes: string | null) => Promise<void>;
  /** Callback when cancel/close is clicked */
  onClose: () => void;
  /** Whether a save is in progress */
  loading?: boolean;
}

export function ColumnNotesForm({
  column,
  tableName: _tableName,
  onSave,
  onClose,
  loading = false,
}: ColumnNotesFormProps) {
  const [notes, setNotes] = useState(column.notes ?? "");

  const handleSave = async () => {
    const trimmedNotes = notes.trim();
    await onSave(trimmedNotes.length === 0 ? null : trimmedNotes);
  };

  const displayName = column.rename ?? column.name;
  const hasRename = column.rename && column.rename !== column.name;

  return (
    <Stack gap="md">
      <Stack gap="xs">
        <Text fw={600}>{displayName}</Text>
        {hasRename && (
          <Badge color="yellow" size="sm" variant="light" w="fit-content">
            Database name: {column.name}
          </Badge>
        )}
        <Text size="sm" c="dimmed">
          Use column notes to add clarifications or transformation hints for
          this field. These notes help the AI interpret the column correctly
          when answering questions.
        </Text>
      </Stack>

      <Textarea
        label="Column Notes"
        description="Plain-language notes shown to the model when this column is used."
        value={notes}
        onChange={(event) => setNotes(event.currentTarget.value)}
        autosize
        minRows={4}
        maxRows={12}
      />

      <Group justify="flex-end" gap="sm">
        <Button variant="outline" onClick={onClose} disabled={loading}>
          Cancel
        </Button>
        <Button onClick={handleSave} loading={loading}>
          Save
        </Button>
      </Group>
    </Stack>
  );
}
