import { Alert, Badge, Group, Text } from "@mantine/core";
import { IconLock } from "@tabler/icons-react";
import type { TableCondition } from "./types";

export interface WhereConditionProps {
  /** The table name */
  tableName: string;
  /** The condition (null if no condition) */
  condition: TableCondition | null;
  /** Compact mode - just shows a badge */
  compact?: boolean;
}

export function WhereCondition({
  tableName,
  condition,
  compact = false,
}: WhereConditionProps) {
  if (!condition) {
    return null;
  }

  if (compact) {
    return (
      <Badge
        size="xs"
        color="blue"
        variant="light"
        leftSection={<IconLock size={10} />}
      >
        RLS
      </Badge>
    );
  }

  return (
    <Alert
      icon={<IconLock size={16} />}
      color="blue"
      variant="light"
      p="xs"
    >
      <Group gap="xs" wrap="wrap" align="center">
        <Text size="sm" fw={500}>
          Row-level security:
        </Text>
        <Text size="sm" component="span">
          Results filtered where
        </Text>
        <Badge size="sm" variant="outline" color="blue">
          {tableName}.{condition.column.name}
        </Badge>
        <Text size="sm" component="span">
          =
        </Text>
        <Badge size="sm" variant="filled" color="blue">
          {"{" + condition.userContextField.key + "}"}
        </Badge>
      </Group>
    </Alert>
  );
}
