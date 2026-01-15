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
import { IconLink, IconTrash } from "@tabler/icons-react";
import type { TableCondition } from "./types";

export interface TableConditionsTableProps {
  /** List of table conditions to display */
  conditions: TableCondition[];
  /** Callback when Add Condition button is clicked */
  onAddClick: () => void;
  /** Callback when delete button is clicked for a condition */
  onDeleteCondition: (id: string) => void;
  /**
   * Optional callback before deletion. Return true to proceed, false to cancel.
   * Use this for confirmation dialogs or validation.
   */
  onBeforeDelete?: (condition: TableCondition) => Promise<boolean> | boolean;
  /** Whether the Add button should be disabled (e.g., no fields or tables exist) */
  disabled?: boolean;
  /** Whether to show loading state */
  loading?: boolean;
  /** ID of condition currently being deleted (shows loading spinner on that row) */
  deletingConditionId?: string;
  /** Custom empty state message */
  emptyMessage?: string;
  /** Custom title (defaults to "Table Conditions") */
  title?: string;
  /** Description text shown below the title */
  description?: string;
}

export function TableConditionsTable({
  conditions,
  onAddClick,
  onDeleteCondition,
  onBeforeDelete,
  disabled = false,
  loading = false,
  deletingConditionId,
  emptyMessage = "No table conditions defined. Add conditions to enable automatic row-level filtering.",
  title = "Table Conditions",
  description = "Map context fields to table columns. Queries will automatically filter rows where the column value matches the context value.",
}: TableConditionsTableProps) {
  const handleDelete = async (condition: TableCondition) => {
    if (onBeforeDelete) {
      const shouldProceed = await onBeforeDelete(condition);
      if (!shouldProceed) return;
    }
    onDeleteCondition(condition.id);
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
          leftSection={<IconLink size={16} />}
          size="xs"
          onClick={onAddClick}
          disabled={disabled}
        >
          Add Condition
        </Button>
      </Group>

      <Text size="sm" c="dimmed" mb="md">
        {description}
      </Text>

      {conditions.length === 0 ? (
        <Text c="dimmed" size="sm">
          {emptyMessage}
        </Text>
      ) : (
        <Table striped>
          <Table.Thead>
            <Table.Tr>
              <Table.Th>Database</Table.Th>
              <Table.Th>Table</Table.Th>
              <Table.Th>Column</Table.Th>
              <Table.Th>=</Table.Th>
              <Table.Th>Context Field</Table.Th>
              <Table.Th w={50}></Table.Th>
            </Table.Tr>
          </Table.Thead>
          <Table.Tbody>
            {conditions.map((condition) => (
              <Table.Tr key={condition.id}>
                <Table.Td>
                  <Text fw={500}>{condition.table.connection?.name ?? "—"}</Text>
                </Table.Td>
                <Table.Td>
                  <Text fw={500}>{condition.table.name}</Text>
                </Table.Td>
                <Table.Td>
                  <Badge size="sm" variant="light">
                    {condition.column.name}
                  </Badge>
                </Table.Td>
                <Table.Td>
                  <Text c="dimmed">=</Text>
                </Table.Td>
                <Table.Td>
                  <Badge size="sm" color="blue" variant="light">
                    {"{" + condition.userContextField.key + "}"}
                  </Badge>
                </Table.Td>
                <Table.Td>
                  <ActionIcon
                    color="red"
                    variant="subtle"
                    onClick={() => handleDelete(condition)}
                    loading={deletingConditionId === condition.id}
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
