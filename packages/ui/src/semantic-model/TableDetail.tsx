"use client";

import { useState } from "react";
import {
  Stack,
  Group,
  Text,
  Title,
  Button,
  Paper,
  Divider,
  ScrollArea,
  LoadingOverlay,
  Box,
  Drawer,
  Badge,
  Textarea,
  ActionIcon,
} from "@mantine/core";
import {
  IconPlus,
  IconLink,
  IconRuler,
  IconFilter,
  IconCheck,
  IconEdit,
} from "@tabler/icons-react";
import type {
  TableSchema,
  Column,
  ComputedColumn,
  Relation,
  UserContextField,
  UserContextStatus,
  TableWithColumns,
  UpdateTablePayload,
  UpdateColumnPayload,
  CreateComputedColumnPayload,
  UpdateComputedColumnPayload,
  CreateManualRelationPayload,
  UpdateManualRelationPayload,
  UpsertContextFilterPayload,
  ColumnConversionCreatePayload,
  ColumnConversionUpdatePayload,
  ColumnValueEnumCreatePayload,
  ColumnValueEnumEntryInput,
  ColumnValueEnumUpdatePayload,
  UnitColumnPayload,
  ComputedColumnUnitPayload,
} from "./types";
import { WhereCondition } from "./WhereCondition";
import { ColumnTable } from "./ColumnTable";
import { RelationTable } from "./RelationTable";
import { ColumnNotesForm } from "./ColumnNotesForm";
import { ComputedColumnNotesForm } from "./ComputedColumnNotesForm";
import { UnitsForm } from "./UnitsForm";
import { ContextFilterForm } from "./ContextFilterForm";
import { ManualRelationForm } from "./ManualRelationForm";
import { ComputedColumnForm } from "./ComputedColumnForm";
import { ColumnConversionForm } from "./ColumnConversionForm";
import { ColumnValueEnumForm } from "./ColumnValueEnumForm";

export interface TableDetailProps {
  /** The table to display */
  table: TableSchema;
  /** User context fields for access constraints */
  userContextFields: UserContextField[];
  /** User context status for access constraints */
  userContextStatus?: UserContextStatus;
  /** Available tables for manual relations */
  availableTables: TableWithColumns[];
  /** Whether the component is loading */
  loading?: boolean;
  /** Whether the editor is in read-only mode (disables all mutations) */
  readOnly?: boolean;
  /** Callback when table access changes */
  onUpdateTable?: (payload: UpdateTablePayload) => Promise<void>;
  /** Callback when a column is updated */
  onUpdateColumn?: (
    columnId: string,
    payload: UpdateColumnPayload,
  ) => Promise<void>;
  /** Callback when a computed column is created */
  onCreateComputedColumn?: (
    payload: CreateComputedColumnPayload,
  ) => Promise<void>;
  /** Callback when a computed column is updated */
  onUpdateComputedColumn?: (
    columnId: string,
    payload: UpdateComputedColumnPayload,
  ) => Promise<void>;
  /** Callback when a computed column is deleted */
  onDeleteComputedColumn?: (columnId: string) => Promise<void>;
  /** Callback when a relation is updated */
  onUpdateRelation?: (relationId: string, selected: boolean) => Promise<void>;
  /** Callback when a manual relation is created */
  onCreateManualRelation?: (
    payload: CreateManualRelationPayload,
  ) => Promise<void>;
  /** Callback when a manual relation is updated */
  onUpdateManualRelation?: (
    relationId: string,
    payload: UpdateManualRelationPayload,
  ) => Promise<void>;
  /** Callback when a manual relation is deleted */
  onDeleteManualRelation?: (relationId: string) => Promise<void>;
  /** Callback when a context filter is created/updated */
  onUpsertContextFilter?: (
    payload: UpsertContextFilterPayload,
  ) => Promise<void>;
  /** Callback when a context filter is deleted */
  onDeleteContextFilter?: () => Promise<void>;
  /** Callback when a column conversion is created */
  onCreateColumnConversion?: (
    columnId: string,
    payload: ColumnConversionCreatePayload,
  ) => Promise<void>;
  /** Callback when a column conversion is updated */
  onUpdateColumnConversion?: (
    columnId: string,
    payload: ColumnConversionUpdatePayload,
  ) => Promise<void>;
  /** Callback when a column conversion is deleted */
  onDeleteColumnConversion?: (columnId: string) => Promise<void>;
  /** Callback when a column enum is created */
  onCreateColumnValueEnum?: (
    columnId: string,
    payload: ColumnValueEnumCreatePayload,
  ) => Promise<void>;
  /** Callback when a column enum is updated */
  onUpdateColumnValueEnum?: (
    columnId: string,
    payload: ColumnValueEnumUpdatePayload,
  ) => Promise<void>;
  /** Callback when a column enum is deleted */
  onDeleteColumnValueEnum?: (columnId: string) => Promise<void>;
  /** Callback to auto-generate enum entries from distinct values */
  onAutoFillColumnValueEnum?: (
    columnId: string,
  ) => Promise<ColumnValueEnumEntryInput[]>;
  /** Callback when a column unit is added */
  onAddColumnUnit?: (payload: UnitColumnPayload) => Promise<void>;
  /** Callback when a computed column unit is updated */
  onUpdateComputedColumnUnit?: (
    payload: ComputedColumnUnitPayload,
  ) => Promise<void>;
}

type ModalState =
  | { type: "none" }
  | { type: "columnNotes"; column: Column }
  | { type: "computedColumnNotes"; column: ComputedColumn }
  | { type: "units" }
  | { type: "contextFilter" }
  | { type: "manualRelation"; relation?: Relation }
  | { type: "computedColumn" }
  | { type: "columnConversion"; column: Column }
  | { type: "columnValueEnum"; column: Column };

export function TableDetail({
  table,
  userContextFields,
  userContextStatus = "UNSET",
  availableTables,
  loading = false,
  readOnly = false,
  onUpdateTable,
  onUpdateColumn,
  onCreateComputedColumn,
  onUpdateComputedColumn,
  onDeleteComputedColumn,
  onUpdateRelation,
  onCreateManualRelation,
  onUpdateManualRelation,
  onDeleteManualRelation,
  onUpsertContextFilter,
  onDeleteContextFilter,
  onCreateColumnConversion,
  onUpdateColumnConversion,
  onDeleteColumnConversion,
  onCreateColumnValueEnum,
  onUpdateColumnValueEnum,
  onDeleteColumnValueEnum,
  onAutoFillColumnValueEnum,
  onAddColumnUnit,
  onUpdateComputedColumnUnit,
}: TableDetailProps) {
  const [modalState, setModalState] = useState<ModalState>({ type: "none" });
  const [isEditingDescription, setIsEditingDescription] = useState(false);
  const [descriptionValue, setDescriptionValue] = useState(table.context ?? "");

  const isDisabled = readOnly || table.access === "OFF";
  const isContextEnabled = userContextStatus === "ENABLED";
  const canEditAccessConstraints = isContextEnabled && !isDisabled;
  const constraintLabel = table.condition
    ? isContextEnabled
      ? "Constraint active"
      : "Constraint inactive"
    : "Add constraint";

  const startEditingDescription = () => {
    setDescriptionValue(table.context ?? "");
    setIsEditingDescription(true);
  };

  const cancelEditingDescription = () => {
    setDescriptionValue(table.context ?? "");
    setIsEditingDescription(false);
  };

  const saveDescription = async () => {
    const trimmedContext = descriptionValue.trim();
    await onUpdateTable?.({
      context: trimmedContext.length === 0 ? null : trimmedContext,
    });
    setIsEditingDescription(false);
  };

  const handleColumnSelectedChange = async (
    columnId: string,
    selected: boolean,
  ) => {
    await onUpdateColumn?.(columnId, { selected });
  };

  const handleColumnRename = async (
    columnId: string,
    rename: string | null,
  ) => {
    await onUpdateColumn?.(columnId, { rename });
  };

  const handleComputedColumnSelectedChange = async (
    columnId: string,
    selected: boolean,
  ) => {
    await onUpdateComputedColumn?.(columnId, { selected });
  };

  const handleComputedColumnRename = async (columnId: string, name: string) => {
    await onUpdateComputedColumn?.(columnId, { name });
  };

  const handleRelationSelectedChange = async (
    relationId: string,
    selected: boolean,
  ) => {
    await onUpdateRelation?.(relationId, selected);
  };

  const closeModal = () => setModalState({ type: "none" });

  const getModalTitle = (): string => {
    switch (modalState.type) {
      case "columnNotes":
        return `Column Notes - ${modalState.column.name}`;
      case "computedColumnNotes":
        return `Computed Column Notes - ${modalState.column.name}`;
      case "units":
        return "Configure Units";
      case "contextFilter":
        return "Row-level access constraint";
      case "manualRelation":
        return modalState.relation ? "Edit Relation" : "Add Relation";
      case "computedColumn":
        return "Add Computed Column";
      case "columnConversion":
        return `Configure Conversion - ${modalState.column.name}`;
      case "columnValueEnum":
        return `Configure Enum - ${modalState.column.name}`;
      default:
        return "";
    }
  };

  const getDrawerSize = (): string => {
    switch (modalState.type) {
      case "computedColumn":
        return "xl";
      case "manualRelation":
      case "columnConversion":
      case "columnValueEnum":
        return "lg";
      default:
        return "md";
    }
  };

  return (
    <Box pos="relative" h="100%">
      <LoadingOverlay visible={loading} />
      <ScrollArea h="100%" p="md">
        <Stack gap="lg">
          {/* Header */}
          <div>
            <Title order={3}>{table.name}</Title>
            {isEditingDescription ? (
              <Stack gap="xs" mt="sm">
                <Textarea
                  placeholder="Add a description to help the AI understand this table..."
                  value={descriptionValue}
                  onChange={(e) => setDescriptionValue(e.currentTarget.value)}
                  autosize
                  minRows={2}
                  maxRows={6}
                  autoFocus
                />
                <Group gap="xs">
                  <Button size="xs" onClick={saveDescription} loading={loading}>
                    Save
                  </Button>
                  <Button
                    size="xs"
                    variant="subtle"
                    onClick={cancelEditingDescription}
                  >
                    Cancel
                  </Button>
                </Group>
              </Stack>
            ) : (
              <Group gap="xs" mt="xs">
                {table.context ? (
                  <Text size="sm" c="dimmed" style={{ flex: 1 }}>
                    {table.context}
                  </Text>
                ) : (
                  <Text size="sm" c="dimmed" fs="italic" style={{ flex: 1 }}>
                    No description
                  </Text>
                )}
                <ActionIcon
                  size="sm"
                  variant="subtle"
                  onClick={startEditingDescription}
                  disabled={isDisabled}
                >
                  <IconEdit size={14} />
                </ActionIcon>
              </Group>
            )}
          </div>

          {/* Where Condition (Access Constraint) */}
          {table.condition && (
            <WhereCondition
              tableName={table.name}
              condition={table.condition}
              userContextStatus={userContextStatus}
            />
          )}

          {/* Action Buttons */}
          <Paper p="sm" withBorder radius="sm">
            <Group justify="space-between" wrap="wrap" gap="md">
              {/* Schema Actions Group */}
              <Group gap="sm">
                <Text size="xs" c="dimmed" fw={500} tt="uppercase">
                  Schema
                </Text>
                <Button.Group>
                  <Button
                    variant="light"
                    size="xs"
                    leftSection={<IconPlus size={14} />}
                    onClick={() => setModalState({ type: "computedColumn" })}
                    disabled={isDisabled}
                  >
                    Computed Column
                    {table.computedColumns.length > 0 && (
                      <Badge
                        size="xs"
                        ml={6}
                        circle
                        variant="filled"
                        color="teal"
                      >
                        {table.computedColumns.length}
                      </Badge>
                    )}
                  </Button>
                  <Button
                    variant="light"
                    size="xs"
                    leftSection={<IconLink size={14} />}
                    onClick={() => setModalState({ type: "manualRelation" })}
                    disabled={isDisabled}
                  >
                    Relation
                    {table.relations.length > 0 && (
                      <Badge
                        size="xs"
                        ml={6}
                        circle
                        variant="filled"
                        color="blue"
                      >
                        {table.relations.length}
                      </Badge>
                    )}
                  </Button>
                  <Button
                    variant="light"
                    size="xs"
                    leftSection={<IconRuler size={14} />}
                    onClick={() => setModalState({ type: "units" })}
                    disabled={isDisabled}
                  >
                    Units
                  </Button>
                </Button.Group>
              </Group>

              {/* Access Constraints Group */}
              <Group gap="sm">
                <Text size="xs" c="dimmed" fw={500} tt="uppercase">
                  Access Constraints
                </Text>
                <Stack gap={2}>
                <Button
                  variant={table.condition ? "filled" : "outline"}
                  color="blue"
                  size="xs"
                  leftSection={<IconFilter size={14} />}
                  rightSection={
                    table.condition ? <IconCheck size={12} /> : undefined
                  }
                  onClick={() => {
                    if (canEditAccessConstraints) {
                      setModalState({ type: "contextFilter" });
                    }
                  }}
                  disabled={!canEditAccessConstraints}
                >
                  {constraintLabel}
                </Button>
                {!isContextEnabled && (
                  <Text size="xs" c="dimmed">
                    Enable user context to apply or edit constraints.
                  </Text>
                  )}
                </Stack>
              </Group>
            </Group>
          </Paper>

          <Divider />

          {/* Columns */}
          <ColumnTable
            columns={table.columns}
            computedColumns={table.computedColumns}
            disabled={isDisabled}
            onColumnSelectedChange={handleColumnSelectedChange}
            onColumnRename={handleColumnRename}
            onColumnNotesClick={(column) =>
              setModalState({ type: "columnNotes", column })
            }
            onColumnConversionClick={(column) =>
              setModalState({ type: "columnConversion", column })
            }
            onColumnValueEnumClick={(column) =>
              setModalState({ type: "columnValueEnum", column })
            }
            onComputedColumnSelectedChange={handleComputedColumnSelectedChange}
            onComputedColumnRename={handleComputedColumnRename}
            onComputedColumnNotesClick={(column) =>
              setModalState({ type: "computedColumnNotes", column })
            }
            onComputedColumnDelete={onDeleteComputedColumn}
          />

          {/* Relations */}
          {table.relations.length > 0 && (
            <>
              <Divider />
              <RelationTable
                relations={table.relations}
                disabled={isDisabled}
                onRelationSelectedChange={handleRelationSelectedChange}
                onRelationEdit={(relation) =>
                  setModalState({ type: "manualRelation", relation })
                }
                onRelationDelete={onDeleteManualRelation}
              />
            </>
          )}
        </Stack>
      </ScrollArea>

      {/* Drawer Container */}
      <Drawer
        opened={modalState.type !== "none"}
        onClose={closeModal}
        title={getModalTitle()}
        size={getDrawerSize()}
        position="right"
        padding="md"
      >
        {modalState.type === "columnNotes" && (
          <ColumnNotesForm
            column={modalState.column}
            tableName={table.name}
            onClose={closeModal}
            onSave={async (notes) => {
              await onUpdateColumn?.(modalState.column.id, { notes });
              closeModal();
            }}
          />
        )}

        {modalState.type === "computedColumnNotes" && (
          <ComputedColumnNotesForm
            computedColumn={modalState.column}
            onClose={closeModal}
            onSave={async (notes) => {
              await onUpdateComputedColumn?.(modalState.column.id, { notes });
              closeModal();
            }}
          />
        )}

        {modalState.type === "units" && (
          <UnitsForm
            table={table}
            onClose={closeModal}
            onSave={async (payload) => {
              await onAddColumnUnit?.(payload);
              closeModal();
            }}
            onSaveComputedColumn={async (payload) => {
              await onUpdateComputedColumnUnit?.(payload);
              closeModal();
            }}
          />
        )}

        {modalState.type === "contextFilter" && (
          <ContextFilterForm
            table={table}
            userContextFields={userContextFields}
            readOnly={readOnly || !isContextEnabled}
            onClose={closeModal}
            onSave={async (payload) => {
              await onUpsertContextFilter?.(payload);
              closeModal();
            }}
            onDelete={async () => {
              await onDeleteContextFilter?.();
              closeModal();
            }}
          />
        )}

        {modalState.type === "manualRelation" && (
          <ManualRelationForm
            table={table}
            availableTables={availableTables}
            relation={modalState.relation}
            onClose={closeModal}
            onCreate={async (payload) => {
              await onCreateManualRelation?.(payload);
              closeModal();
            }}
            onUpdate={async (relationId, payload) => {
              await onUpdateManualRelation?.(relationId, payload);
              closeModal();
            }}
          />
        )}

        {modalState.type === "computedColumn" && (
          <ComputedColumnForm
            table={table}
            onClose={closeModal}
            onSave={async (payload) => {
              await onCreateComputedColumn?.(payload);
              closeModal();
            }}
          />
        )}

        {modalState.type === "columnConversion" && (
          <ColumnConversionForm
            column={modalState.column}
            onClose={closeModal}
            onCreate={async (payload) => {
              await onCreateColumnConversion?.(modalState.column.id, payload);
              closeModal();
            }}
            onUpdate={async (conversionId, payload) => {
              await onUpdateColumnConversion?.(modalState.column.id, payload);
              closeModal();
            }}
            onDelete={async () => {
              await onDeleteColumnConversion?.(modalState.column.id);
              closeModal();
            }}
          />
        )}

        {modalState.type === "columnValueEnum" && (
          <ColumnValueEnumForm
            column={modalState.column}
            onClose={closeModal}
            onCreate={async (payload) => {
              await onCreateColumnValueEnum?.(modalState.column.id, payload);
              closeModal();
            }}
            onUpdate={async (payload) => {
              await onUpdateColumnValueEnum?.(modalState.column.id, payload);
              closeModal();
            }}
            onDelete={async () => {
              await onDeleteColumnValueEnum?.(modalState.column.id);
              closeModal();
            }}
            onAutoFill={async () => {
              return (
                (await onAutoFillColumnValueEnum?.(modalState.column.id)) ?? []
              );
            }}
          />
        )}
      </Drawer>
    </Box>
  );
}
