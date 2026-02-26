"use client";

import { useEffect, useState } from "react";
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
  Collapse,
  Drawer,
  Badge,
  Textarea,
  ActionIcon,
  Alert,
} from "@mantine/core";
import {
  IconPlus,
  IconLink,
  IconRuler,
  IconFilter,
  IconLock,
  IconCheck,
  IconEdit,
  IconRefresh,
  IconTrash,
  IconChevronDown,
  IconChevronUp,
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
  UpsertTableAccessPolicyPayload,
  ColumnConversionCreatePayload,
  ColumnConversionUpdatePayload,
  ColumnValueEnumCreatePayload,
  ColumnValueEnumEntryInput,
  ColumnValueEnumUpdatePayload,
  UnitColumnPayload,
  ComputedColumnUnitPayload,
  VirtualTableValidationResult,
  UpdateVirtualTableSqlPayload,
  VirtualTableColumnRefreshResult,
  VirtualTableDialect,
} from "./types";
import { WhereCondition } from "./WhereCondition";
import { ColumnTable } from "./ColumnTable";
import { RelationTable } from "./RelationTable";
import { ColumnNotesForm } from "./ColumnNotesForm";
import { ComputedColumnNotesForm } from "./ComputedColumnNotesForm";
import { UnitsForm } from "./UnitsForm";
import { ContextFilterForm } from "./ContextFilterForm";
import { TableAccessPolicyForm } from "./TableAccessPolicyForm";
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
  /** Callback when a table access policy is created/updated */
  onUpsertTableAccessPolicy?: (
    payload: UpsertTableAccessPolicyPayload,
  ) => Promise<void>;
  /** Callback when a table access policy is deleted */
  onDeleteTableAccessPolicy?: () => Promise<void>;
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
  /** Currently selected connection ID (required for virtual SQL validation) */
  selectedConnectionId?: string | null;
  /** Validate virtual table SQL (virtual tables only) */
  onValidateVirtualTableSql?: (payload: {
    connectionId: string;
    sql: string;
    dialect?: VirtualTableDialect;
    previewLimit?: number;
  }) => Promise<VirtualTableValidationResult>;
  /** Update virtual table SQL/config (virtual tables only) */
  onUpdateVirtualTableSql?: (
    tableId: string,
    payload: UpdateVirtualTableSqlPayload,
  ) => Promise<{ tableId: string } | void>;
  /** Refresh virtual table inferred columns (virtual tables only) */
  onRefreshVirtualTableColumns?: (
    tableId: string,
  ) => Promise<VirtualTableColumnRefreshResult | void>;
  /** Delete virtual table (virtual tables only) */
  onDeleteVirtualTable?: (tableId: string) => Promise<void>;
}

type ModalState =
  | { type: "none" }
  | { type: "columnNotes"; column: Column }
  | { type: "computedColumnNotes"; column: ComputedColumn }
  | { type: "units" }
  | { type: "contextFilter" }
  | { type: "accessPolicy" }
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
  onUpsertTableAccessPolicy,
  onDeleteTableAccessPolicy,
  onCreateColumnConversion,
  onUpdateColumnConversion,
  onDeleteColumnConversion,
  onCreateColumnValueEnum,
  onUpdateColumnValueEnum,
  onDeleteColumnValueEnum,
  onAutoFillColumnValueEnum,
  onAddColumnUnit,
  onUpdateComputedColumnUnit,
  onUpdateVirtualTableSql,
  onRefreshVirtualTableColumns,
  onDeleteVirtualTable,
}: TableDetailProps) {
  const [modalState, setModalState] = useState<ModalState>({ type: "none" });
  const [isEditingDescription, setIsEditingDescription] = useState(false);
  const [descriptionValue, setDescriptionValue] = useState(table.context ?? "");
  const [virtualSqlValue, setVirtualSqlValue] = useState(
    table.virtualTableConfig?.sql ?? "",
  );
  const [virtualValidationResult, setVirtualValidationResult] =
    useState<VirtualTableValidationResult | null>(null);
  const [virtualMutationError, setVirtualMutationError] = useState<string | null>(
    null,
  );
  const [virtualAction, setVirtualAction] = useState<
    "save" | "refresh" | "delete" | null
  >(null);
  const [isVirtualSqlExpanded, setIsVirtualSqlExpanded] = useState(false);

  const isDisabled = readOnly || table.access === "OFF";
  const isVirtualTable = table.source === "VIRTUAL";
  const virtualSqlDisabled = readOnly;
  const isContextEnabled = userContextStatus === "ENABLED";
  const canEditAccessConstraints = isContextEnabled && !isDisabled;
  const constraintLabel = table.condition
    ? isContextEnabled
      ? "Constraint active"
      : "Constraint inactive"
    : "Add constraint";
  const accessPolicyLabel = table.accessPolicy
    ? isContextEnabled
      ? "Policy active"
      : "Policy inactive"
    : "Add policy";

  useEffect(() => {
    setDescriptionValue(table.context ?? "");
    setVirtualSqlValue(table.virtualTableConfig?.sql ?? "");
    setVirtualValidationResult(null);
    setVirtualMutationError(null);
    setVirtualAction(null);
  }, [
    table.id,
    table.context,
    table.virtualTableConfig?.id,
    table.virtualTableConfig?.sql,
  ]);

  useEffect(() => {
    setIsVirtualSqlExpanded(false);
  }, [table.id]);

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

  const handleSaveVirtualSql = async () => {
    if (!isVirtualTable || !onUpdateVirtualTableSql) return;
    if (!virtualSqlValue.trim()) {
      setVirtualMutationError("SQL is required.");
      return;
    }

    setVirtualAction("save");
    setVirtualMutationError(null);
    try {
      await onUpdateVirtualTableSql(table.id, {
        sql: virtualSqlValue,
      });
    } catch (error) {
      setVirtualMutationError(
        error instanceof Error ? error.message : "Failed to save virtual SQL.",
      );
    } finally {
      setVirtualAction(null);
    }
  };

  const handleRefreshVirtualColumns = async () => {
    if (!isVirtualTable || !onRefreshVirtualTableColumns) return;
    setVirtualAction("refresh");
    setVirtualMutationError(null);
    try {
      const result = await onRefreshVirtualTableColumns(table.id);
      if (result) {
        setVirtualValidationResult({
          ok: true,
          columns: [],
          previewRows: result.previewRows,
        });
      }
    } catch (error) {
      setVirtualMutationError(
        error instanceof Error ? error.message : "Failed to refresh columns.",
      );
    } finally {
      setVirtualAction(null);
    }
  };

  const handleDeleteVirtualTableClick = async () => {
    if (!isVirtualTable || !onDeleteVirtualTable) return;
    if (
      typeof window !== "undefined" &&
      !window.confirm(`Delete virtual table "${table.name}"?`)
    ) {
      return;
    }

    setVirtualAction("delete");
    setVirtualMutationError(null);
    try {
      await onDeleteVirtualTable(table.id);
    } catch (error) {
      setVirtualMutationError(
        error instanceof Error ? error.message : "Failed to delete virtual table.",
      );
    } finally {
      setVirtualAction(null);
    }
  };

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
      case "accessPolicy":
        return "Table access policy";
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
            <Group justify="space-between" align="flex-start" wrap="nowrap" gap="sm">
              <div style={{ flex: 1, minWidth: 0 }}>
                <Title order={3}>{table.name}</Title>
                <Group gap="xs" mt={4}>
                  <Badge
                    size="sm"
                    variant="light"
                    color={table.source === "VIRTUAL" ? "violet" : "gray"}
                  >
                    {table.source === "VIRTUAL"
                      ? "Virtual Table"
                      : "Physical Table"}
                  </Badge>
                  {table.access === "OFF" && (
                    <Badge size="sm" variant="outline" color="gray">
                      Off
                    </Badge>
                  )}
                </Group>
              </div>
              {isVirtualTable && (
                <Button
                  size="xs"
                  color="red"
                  variant="light"
                  leftSection={<IconTrash size={14} />}
                  onClick={handleDeleteVirtualTableClick}
                  loading={virtualAction === "delete"}
                  disabled={virtualSqlDisabled}
                >
                  Delete
                </Button>
              )}
            </Group>
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

          {isVirtualTable && (
            <Paper p="sm" withBorder radius="sm">
              <Stack gap="sm">
                <Group justify="space-between" align="center">
                  <Button
                    size="xs"
                    variant="subtle"
                    onClick={() => setIsVirtualSqlExpanded((prev) => !prev)}
                    rightSection={
                      isVirtualSqlExpanded ? (
                        <IconChevronUp size={14} />
                      ) : (
                        <IconChevronDown size={14} />
                      )
                    }
                  >
                    {isVirtualSqlExpanded ? "Hide SQL" : "Show SQL"}
                  </Button>
                  <Group gap="xs">
                    <Badge variant="light" color="violet">
                      {table.virtualTableConfig?.dialect ?? "unknown"}
                    </Badge>
                  </Group>
                </Group>

                <Collapse in={isVirtualSqlExpanded}>
                  <Stack gap="sm">
                    {!table.virtualTableConfig ? (
                      <Alert color="red">
                        Virtual table configuration is missing for this table.
                      </Alert>
                    ) : (
                      <>
                        <Textarea
                          label="SQL"
                          placeholder="SELECT ..."
                          value={virtualSqlValue}
                          onChange={(e) => {
                            setVirtualSqlValue(e.currentTarget.value);
                            setVirtualValidationResult(null);
                            setVirtualMutationError(null);
                          }}
                          autosize
                          minRows={5}
                          maxRows={14}
                          disabled={virtualSqlDisabled}
                          styles={{ input: { fontFamily: "monospace" } }}
                        />

                        {virtualMutationError && (
                          <Alert color="red">{virtualMutationError}</Alert>
                        )}

                        <Group justify="flex-end" wrap="wrap">
                          <Group gap="xs">
                            <Button
                              size="xs"
                              leftSection={<IconCheck size={14} />}
                              onClick={handleSaveVirtualSql}
                              loading={virtualAction === "save"}
                              disabled={virtualSqlDisabled}
                            >
                              Save SQL
                            </Button>
                            <Button
                              size="xs"
                              variant="light"
                              leftSection={<IconRefresh size={14} />}
                              onClick={handleRefreshVirtualColumns}
                              loading={virtualAction === "refresh"}
                              disabled={virtualSqlDisabled}
                            >
                              Refresh Columns
                            </Button>
                          </Group>
                        </Group>
                      </>
                    )}
                  </Stack>
                </Collapse>
              </Stack>
            </Paper>
          )}

          {/* Where Condition (Access Constraint) */}
          {table.condition && (
            <WhereCondition
              tableName={table.name}
              condition={table.condition}
              userContextStatus={userContextStatus}
            />
          )}

          {table.accessPolicy && (
            <Paper p="sm" withBorder radius="sm">
              <Text size="sm">
                Table access policy: Available when{" "}
                <strong>
                  userContext.{table.accessPolicy.userContextField.key}
                </strong>{" "}
                equals <strong>true</strong>.
              </Text>
            </Paper>
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
                  <Group gap="xs">
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
                    <Button
                      variant={table.accessPolicy ? "filled" : "outline"}
                      color="blue"
                      size="xs"
                      leftSection={<IconLock size={14} />}
                      rightSection={
                        table.accessPolicy ? <IconCheck size={12} /> : undefined
                      }
                      onClick={() => {
                        if (canEditAccessConstraints) {
                          setModalState({ type: "accessPolicy" });
                        }
                      }}
                      disabled={!canEditAccessConstraints}
                    >
                      {accessPolicyLabel}
                    </Button>
                  </Group>
                  {!isContextEnabled && (
                    <Text size="xs" c="dimmed">
                      Enable user context to apply or edit access constraints.
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

        {modalState.type === "accessPolicy" && (
          <TableAccessPolicyForm
            table={table}
            userContextFields={userContextFields}
            readOnly={readOnly || !isContextEnabled}
            onClose={closeModal}
            onSave={async (payload) => {
              await onUpsertTableAccessPolicy?.(payload);
              closeModal();
            }}
            onDelete={async () => {
              await onDeleteTableAccessPolicy?.();
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
