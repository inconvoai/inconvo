"use client";

import { useState } from "react";
import {
  Table,
  Checkbox,
  Badge,
  Group,
  Text,
  ActionIcon,
  Tooltip,
  Box,
} from "@mantine/core";
import {
  IconChevronUp,
  IconChevronDown,
  IconEdit,
  IconTrash,
  IconAlertTriangle,
} from "@tabler/icons-react";
import type { Relation, TableAccess } from "./types";

export interface RelationTableProps {
  /** Relations to display */
  relations: Relation[];
  /** Whether the table is disabled (access = OFF) */
  disabled?: boolean;
  /** Callback when a relation's selected state changes */
  onRelationSelectedChange?: (relationId: string, selected: boolean) => void;
  /** Callback when edit is clicked on a manual relation */
  onRelationEdit?: (relation: Relation) => void;
  /** Callback when delete is clicked on a manual relation */
  onRelationDelete?: (relationId: string) => void;
}

function getAccessColor(access: TableAccess): string {
  switch (access) {
    case "QUERYABLE":
      return "green";
    case "JOINABLE":
      return "orange";
    case "OFF":
      return "gray";
    default:
      return "gray";
  }
}

export function RelationTable({
  relations,
  disabled = false,
  onRelationSelectedChange,
  onRelationEdit,
  onRelationDelete,
}: RelationTableProps) {
  const [activeExpanded, setActiveExpanded] = useState(true);
  const [inactiveExpanded, setInactiveExpanded] = useState(false);
  const [disabledExpanded, setDisabledExpanded] = useState(false);

  // Group relations by status
  const activeRelations = relations.filter(
    (rel) => rel.selected && rel.targetTable.access !== "OFF"
  );
  const inactiveRelations = relations.filter(
    (rel) => !rel.selected && rel.targetTable.access !== "OFF"
  );
  const disabledRelations = relations.filter(
    (rel) => rel.targetTable.access === "OFF"
  );

  const formatColumnMappings = (relation: Relation): string | null => {
    if (!relation.columnMappings || relation.columnMappings.length === 0) {
      return null;
    }
    return relation.columnMappings
      .map(
        (mapping) =>
          `${mapping.sourceColumnName ?? "?"} → ${mapping.targetColumnName ?? "?"}`
      )
      .join(", ");
  };

  const renderRelationRow = (relation: Relation, isTargetOff: boolean = false) => {
    const mappingInfo = formatColumnMappings(relation);
    const isManual = relation.source === "MANUAL";
    const isBroken = relation.status === "BROKEN";

    return (
      <Table.Tr key={relation.id}>
        <Table.Td>
          <Checkbox
            disabled={disabled || isTargetOff}
            checked={relation.selected}
            onChange={(event) =>
              onRelationSelectedChange?.(relation.id, event.currentTarget.checked)
            }
          />
        </Table.Td>
        <Table.Td>
          <Group gap="xs">
            <Text size="sm">{relation.name}</Text>
            {isManual && (
              <Badge size="xs" color="violet" variant="light">
                Manual
              </Badge>
            )}
            {isBroken && (
              <Tooltip
                label={
                  relation.errorTag ??
                  "One or more mapped columns are missing from the schema."
                }
              >
                <Badge size="xs" color="red" variant="light" leftSection={<IconAlertTriangle size={10} />}>
                  Broken
                </Badge>
              </Tooltip>
            )}
          </Group>
        </Table.Td>
        <Table.Td>
          <Group gap="xs">
            <Text size="sm">{relation.targetTable.name}</Text>
            <Badge size="xs" color={getAccessColor(relation.targetTable.access)} variant="light">
              {relation.targetTable.access}
            </Badge>
            {relation.isList && (
              <Badge size="xs" color="cyan" variant="light">
                List
              </Badge>
            )}
          </Group>
        </Table.Td>
        <Table.Td>
          {mappingInfo && (
            <Text size="xs" c="dimmed">
              {mappingInfo}
            </Text>
          )}
        </Table.Td>
        <Table.Td>
          {isManual && (
            <Group gap={4}>
              <Tooltip label="Edit relation">
                <ActionIcon
                  size="sm"
                  variant="subtle"
                  disabled={disabled}
                  onClick={() => onRelationEdit?.(relation)}
                >
                  <IconEdit size={14} />
                </ActionIcon>
              </Tooltip>
              <Tooltip label="Delete relation">
                <ActionIcon
                  size="sm"
                  variant="subtle"
                  color="red"
                  disabled={disabled}
                  onClick={() => onRelationDelete?.(relation.id)}
                >
                  <IconTrash size={14} />
                </ActionIcon>
              </Tooltip>
            </Group>
          )}
        </Table.Td>
      </Table.Tr>
    );
  };

  if (relations.length === 0) {
    return null;
  }

  return (
    <Box>
      <Group align="center" mb="xs">
        <Badge color="blue" size="sm">
          Relations
        </Badge>
      </Group>
      <Table striped highlightOnHover verticalSpacing={2} horizontalSpacing="xs">
        <Table.Thead>
          <Table.Tr>
            <Table.Th w={80}>On</Table.Th>
            <Table.Th w="25%">Name</Table.Th>
            <Table.Th w="25%">Target</Table.Th>
            <Table.Th w="30%">Mapping</Table.Th>
            <Table.Th w={80}>Actions</Table.Th>
          </Table.Tr>
        </Table.Thead>
        <Table.Tbody>
          {/* Active relations section */}
          {activeRelations.length > 0 && (
            <>
              <Table.Tr
                style={{ cursor: "pointer", backgroundColor: "var(--mantine-color-gray-0)" }}
                onClick={() => setActiveExpanded(!activeExpanded)}
              >
                <Table.Td colSpan={5}>
                  <Group gap="xs">
                    <Text fw={500} fz="sm">
                      Active Relations ({activeRelations.length})
                    </Text>
                    {activeExpanded ? (
                      <IconChevronUp size={16} />
                    ) : (
                      <IconChevronDown size={16} />
                    )}
                  </Group>
                </Table.Td>
              </Table.Tr>
              {activeExpanded && activeRelations.map((rel) => renderRelationRow(rel))}
            </>
          )}

          {/* Inactive relations section */}
          {inactiveRelations.length > 0 && (
            <>
              <Table.Tr
                style={{ cursor: "pointer", backgroundColor: "var(--mantine-color-gray-0)" }}
                onClick={() => setInactiveExpanded(!inactiveExpanded)}
              >
                <Table.Td colSpan={5}>
                  <Group gap="xs">
                    <Text fw={500} fz="sm" c="dimmed">
                      Inactive Relations ({inactiveRelations.length})
                    </Text>
                    {inactiveExpanded ? (
                      <IconChevronUp size={16} />
                    ) : (
                      <IconChevronDown size={16} />
                    )}
                  </Group>
                </Table.Td>
              </Table.Tr>
              {inactiveExpanded && inactiveRelations.map((rel) => renderRelationRow(rel))}
            </>
          )}

          {/* Disabled relations section (target table is OFF) */}
          {disabledRelations.length > 0 && (
            <>
              <Table.Tr
                style={{ cursor: "pointer", backgroundColor: "var(--mantine-color-gray-0)" }}
                onClick={() => setDisabledExpanded(!disabledExpanded)}
              >
                <Table.Td colSpan={5}>
                  <Group gap="xs">
                    <Text fw={500} fz="sm" c="dimmed">
                      Disabled Relations ({disabledRelations.length})
                    </Text>
                    <Text fz="xs" c="dimmed">
                      (target table is OFF)
                    </Text>
                    {disabledExpanded ? (
                      <IconChevronUp size={16} />
                    ) : (
                      <IconChevronDown size={16} />
                    )}
                  </Group>
                </Table.Td>
              </Table.Tr>
              {disabledExpanded && disabledRelations.map((rel) => renderRelationRow(rel, true))}
            </>
          )}
        </Table.Tbody>
      </Table>
    </Box>
  );
}
