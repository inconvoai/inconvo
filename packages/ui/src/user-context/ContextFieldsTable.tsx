"use client";

import {
  Paper,
  Group,
  Button,
  Table,
  Badge,
  ActionIcon,
  Text,
  Title,
  Loader,
  Center,
} from "@mantine/core";
import { IconPlus, IconTrash } from "@tabler/icons-react";
import type { ContextField } from "./types";

export interface ContextFieldsTableProps {
  /** List of context fields to display */
  fields: ContextField[];
  /** Callback when Add Field button is clicked */
  onAddClick: () => void;
  /** Callback when delete button is clicked for a field */
  onDeleteField: (id: string) => void;
  /**
   * Optional callback before deletion. Return true to proceed, false to cancel.
   * Use this for confirmation dialogs or validation.
   * If not provided, deletion proceeds immediately.
   */
  onBeforeDelete?: (field: ContextField) => Promise<boolean> | boolean;
  /** Whether to show loading state */
  loading?: boolean;
  /** ID of field currently being deleted (shows loading spinner on that row) */
  deletingFieldId?: string;
  /** Custom empty state message */
  emptyMessage?: string;
  /** Custom title (defaults to "Context Fields") */
  title?: string;
}

export function ContextFieldsTable({
  fields,
  onAddClick,
  onDeleteField,
  onBeforeDelete,
  loading = false,
  deletingFieldId,
  emptyMessage = 'No context fields defined. Add fields like "user_id" or "tenant_id" to enable row-level security.',
  title = "Context Fields",
}: ContextFieldsTableProps) {
  const handleDelete = async (field: ContextField) => {
    if (onBeforeDelete) {
      const shouldProceed = await onBeforeDelete(field);
      if (!shouldProceed) return;
    }
    onDeleteField(field.id);
  };
  if (loading) {
    return (
      <Paper withBorder p="md">
        <Center py="xl">
          <Loader size="sm" />
        </Center>
      </Paper>
    );
  }

  return (
    <Paper withBorder p="md">
      <Group justify="space-between" mb="md">
        <Title order={4}>{title}</Title>
        <Button
          leftSection={<IconPlus size={16} />}
          size="xs"
          onClick={onAddClick}
        >
          Add Field
        </Button>
      </Group>

      {fields.length === 0 ? (
        <Text c="dimmed" size="sm">
          {emptyMessage}
        </Text>
      ) : (
        <Table striped>
          <Table.Thead>
            <Table.Tr>
              <Table.Th>Key</Table.Th>
              <Table.Th>Type</Table.Th>
              <Table.Th>Used In Tables:</Table.Th>
              <Table.Th w={50}></Table.Th>
            </Table.Tr>
          </Table.Thead>
          <Table.Tbody>
            {fields.map((field) => (
              <Table.Tr key={field.id}>
                <Table.Td>
                  <Text fw={500}>{field.key}</Text>
                </Table.Td>
                <Table.Td>
                  <Badge size="sm" variant="light">
                    {field.type}
                  </Badge>
                </Table.Td>
                <Table.Td>
                  {field.tableConditions && field.tableConditions.length > 0 ? (
                    <Group gap="xs">
                      {field.tableConditions.map((tc) => (
                        <Badge key={tc.id} size="xs" variant="outline">
                          {tc.tableName}
                        </Badge>
                      ))}
                    </Group>
                  ) : (
                    <Text size="sm" c="dimmed">
                      Not used
                    </Text>
                  )}
                </Table.Td>
                <Table.Td>
                  <ActionIcon
                    color="red"
                    variant="subtle"
                    onClick={() => handleDelete(field)}
                    loading={deletingFieldId === field.id}
                  >
                    <IconTrash size={16} />
                  </ActionIcon>
                </Table.Td>
              </Table.Tr>
            ))}
          </Table.Tbody>
        </Table>
      )}
    </Paper>
  );
}
