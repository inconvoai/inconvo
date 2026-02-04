import { Alert, Badge, Group, Text } from "@mantine/core";
import { IconLock, IconLockOff } from "@tabler/icons-react";
import type { TableCondition, UserContextStatus } from "./types";

export interface WhereConditionProps {
  /** The table name */
  tableName: string;
  /** The condition (null if no condition) */
  condition: TableCondition | null;
  /** Compact mode - just shows a badge */
  compact?: boolean;
  /** User context status for access constraints */
  userContextStatus?: UserContextStatus;
}

export function WhereCondition({
  tableName,
  condition,
  compact = false,
  userContextStatus = "UNSET",
}: WhereConditionProps) {
  if (!condition) {
    return null;
  }

  const isActive = userContextStatus === "ENABLED";
  const statusLabel = isActive
    ? "Access constraint active"
    : userContextStatus === "DISABLED"
      ? "Access constraint inactive (user context disabled)"
      : "Access constraint inactive (user context not configured)";

  if (compact) {
    return (
      <Badge
        size="xs"
        color={isActive ? "blue" : "gray"}
        variant="light"
        leftSection={
          isActive ? <IconLock size={10} /> : <IconLockOff size={10} />
        }
      >
        Constraint
      </Badge>
    );
  }

  return (
    <Alert
      icon={isActive ? <IconLock size={16} /> : <IconLockOff size={16} />}
      color={isActive ? "blue" : "gray"}
      variant="light"
      p="xs"
    >
      <Group gap="xs" wrap="wrap" align="center">
        <Text size="sm" fw={500}>
          {statusLabel}:
        </Text>
        <Text size="sm" component="span">
          Results filtered where
        </Text>
        <Badge size="sm" variant="outline" color={isActive ? "blue" : "gray"}>
          {tableName}.{condition.column.name}
        </Badge>
        <Text size="sm" component="span">
          =
        </Text>
        <Badge size="sm" variant="filled" color={isActive ? "blue" : "gray"}>
          {"{" + condition.userContextField.key + "}"}
        </Badge>
      </Group>
    </Alert>
  );
}
