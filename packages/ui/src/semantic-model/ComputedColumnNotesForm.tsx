import { useState } from "react";
import { Stack, Text, Textarea, Group, Button, Badge } from "@mantine/core";
import type { ComputedColumn } from "./types";

export interface ComputedColumnNotesFormProps {
  /** The computed column being edited */
  computedColumn: Pick<ComputedColumn, "id" | "name" | "notes">;
  /** Callback when save is clicked */
  onSave: (notes: string | null) => Promise<void>;
  /** Callback when cancel/close is clicked */
  onClose: () => void;
  /** Whether a save is in progress */
  loading?: boolean;
}

export function ComputedColumnNotesForm({
  computedColumn,
  onSave,
  onClose,
  loading = false,
}: ComputedColumnNotesFormProps) {
  const [notes, setNotes] = useState(computedColumn.notes ?? "");

  const handleSave = async () => {
    const trimmedNotes = notes.trim();
    await onSave(trimmedNotes.length === 0 ? null : trimmedNotes);
  };

  return (
    <Stack gap="md">
      <Stack gap="xs">
        <Text fw={600}>{computedColumn.name}</Text>
        <Badge color="teal" size="sm" variant="light" w="fit-content">
          Computed column
        </Badge>
        <Text size="sm" c="dimmed">
          Use notes to capture business logic, caveats, or transformation hints
          for this computed column. The model reads these notes when deciding
          how to use the column.
        </Text>
      </Stack>

      <Textarea
        label="Computed Column Notes"
        description="Plain-language guidance shown to the model when this computed column is used."
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
