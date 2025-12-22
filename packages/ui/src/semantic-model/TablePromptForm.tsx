import { useState } from "react";
import { Textarea, Button, Group, Stack, Text, Alert } from "@mantine/core";
import { IconInfoCircle } from "@tabler/icons-react";
import type { TableSchema } from "./types";

export interface TablePromptFormProps {
  /** The table being edited */
  table: Pick<TableSchema, "id" | "name" | "context">;
  /** Callback when save is clicked */
  onSave: (context: string | null) => Promise<void>;
  /** Callback when cancel/close is clicked */
  onClose: () => void;
  /** Whether a save is in progress */
  loading?: boolean;
}

export function TablePromptForm({
  table,
  onSave,
  onClose,
  loading = false,
}: TablePromptFormProps) {
  const [context, setContext] = useState(table.context ?? "");

  const handleSave = async () => {
    const trimmedContext = context.trim();
    await onSave(trimmedContext.length === 0 ? null : trimmedContext);
  };

  return (
    <Stack>
      <Alert
        icon={<IconInfoCircle size={16} />}
        title="Table Prompt"
        color="blue"
      >
        <Text size="sm">
          A block of plain-language text containing contextual rules for the
          AI&apos;s interpretation of the table&apos;s data.
        </Text>
        <Text size="sm" mt="xs" c="dimmed" style={{ fontStyle: "italic" }}>
          E.g. &quot;Products are also referred to as items&quot; or &quot;A
          customer is considered active if they have made a purchase in the last
          30 days&quot;
        </Text>
      </Alert>

      <Textarea
        label="Table Prompt"
        value={context}
        onChange={(event) => setContext(event.currentTarget.value)}
        autosize
        minRows={4}
        maxRows={12}
      />

      <Group justify="flex-end" gap="sm" mt="md">
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
