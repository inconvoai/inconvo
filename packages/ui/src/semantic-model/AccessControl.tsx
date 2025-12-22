import { SegmentedControl, Tooltip, Group, Text } from "@mantine/core";
import {
  IconSearch,
  IconCirclesRelation,
  IconCircleOff,
} from "@tabler/icons-react";
import type { TableAccess } from "./types";

const accessOptions = [
  {
    value: "QUERYABLE" as TableAccess,
    label: "Query",
    description: "Table can be directly queried",
    icon: IconSearch,
    color: "green",
  },
  {
    value: "JOINABLE" as TableAccess,
    label: "Join",
    description: "Only accessible through relations",
    icon: IconCirclesRelation,
    color: "orange",
  },
  {
    value: "OFF" as TableAccess,
    label: "Off",
    description: "Table is hidden from the API",
    icon: IconCircleOff,
    color: "gray",
  },
];

export interface AccessControlProps {
  /** Current access level */
  value: TableAccess;
  /** Callback when access level changes */
  onChange: (value: TableAccess) => void;
  /** Whether the control is disabled */
  disabled?: boolean;
  /** Size of the control */
  size?: "xs" | "sm" | "md";
  /** Whether to show text labels alongside icons */
  showLabels?: boolean;
  /** Whether the control should fill its container */
  fullWidth?: boolean;
}

export function AccessControl({
  value,
  onChange,
  disabled = false,
  size = "sm",
  showLabels = true,
  fullWidth = false,
}: AccessControlProps) {
  const handleChange = (newValue: string) => {
    onChange(newValue as TableAccess);
  };

  const iconSize = size === "xs" ? 12 : size === "sm" ? 14 : 16;

  return (
    <SegmentedControl
      size={size}
      value={value}
      onChange={handleChange}
      disabled={disabled}
      fullWidth={fullWidth}
      data={accessOptions.map((opt) => ({
        value: opt.value,
        label: (
          <Tooltip label={opt.description} position="bottom" withArrow>
            <Group gap={6} wrap="nowrap" justify="center">
              <opt.icon size={iconSize} />
              {showLabels && <Text size={size}>{opt.label}</Text>}
            </Group>
          </Tooltip>
        ),
      }))}
      color={
        value === "QUERYABLE"
          ? "green"
          : value === "JOINABLE"
            ? "orange"
            : "gray"
      }
    />
  );
}
