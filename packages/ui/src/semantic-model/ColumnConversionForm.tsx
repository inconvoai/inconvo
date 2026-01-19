"use client";

import {
  Alert,
  Box,
  Button,
  Group,
  HoverCard,
  List,
  Select,
  Stack,
  Switch,
  Text,
  TextInput,
} from "@mantine/core";
import { useForm } from "@mantine/form";
import { IconInfoCircle } from "@tabler/icons-react";
import { useMemo, useState } from "react";
import {
  type LogicalCastType,
  type SQLCastExpressionAst,
  SQLCastExpressionAstSchema,
} from "@repo/types";
import type {
  Column,
  ColumnConversionCreatePayload,
  ColumnConversionUpdatePayload,
} from "./types";

const logicalCastTypeOptions: { value: LogicalCastType; label: string }[] = [
  { value: "number", label: "Number" },
  { value: "integer", label: "Integer" },
  { value: "bigint", label: "BigInt" },
  { value: "decimal", label: "Decimal" },
  { value: "float", label: "Float" },
];

type ConversionState = {
  type: LogicalCastType | "";
  fallback: string;
  enabled: boolean;
  supported: boolean;
};

function parseFallbackValue(raw: string) {
  const trimmed = raw.trim();
  if (!trimmed) return undefined;
  if (trimmed === "null") return undefined;
  if (trimmed === "true") return true;
  if (trimmed === "false") return false;
  const asNumber = Number(trimmed);
  if (!Number.isNaN(asNumber)) {
    return asNumber;
  }
  return trimmed;
}

function stringifyFallbackValue(value: unknown) {
  if (value === null || value === undefined) return "";
  if (typeof value === "object" || typeof value === "symbol") return "";
  if (
    typeof value === "string" ||
    typeof value === "number" ||
    typeof value === "boolean"
  ) {
    return String(value);
  }
  return "";
}

function buildCastAst(
  columnName: string,
  castType: LogicalCastType,
  fallback: string,
): SQLCastExpressionAst {
  const castAst: SQLCastExpressionAst = {
    type: "cast",
    as: castType,
    expression: {
      type: "column",
      name: columnName,
    },
  };

  const parsedFallback = parseFallbackValue(fallback);
  if (parsedFallback === undefined) {
    return castAst;
  }

  return {
    type: "coalesce",
    expression: castAst,
    fallback: {
      type: "value",
      value: parsedFallback,
    },
  };
}

function deriveConversionState(column: Column): ConversionState {
  const normalizeType = (value?: string | null): LogicalCastType | "" =>
    value && ["number", "integer", "bigint", "decimal", "float"].includes(value)
      ? (value as LogicalCastType)
      : "";

  const conversion = column.conversion;
  if (!conversion?.ast) {
    return {
      type: "",
      fallback: "",
      enabled: true,
      supported: true,
    };
  }

  const parsed = SQLCastExpressionAstSchema.safeParse(conversion.ast);
  if (!parsed.success) {
    return {
      type: normalizeType(conversion.type),
      fallback: "",
      enabled: conversion.selected ?? true,
      supported: false,
    };
  }

  const ast = parsed.data;
  if (ast.type === "cast" && ast.expression.type === "column") {
    return {
      type: ast.as,
      fallback: "",
      enabled: conversion.selected ?? true,
      supported: true,
    };
  }

  if (
    ast.type === "coalesce" &&
    ast.expression.type === "cast" &&
    ast.expression.expression.type === "column" &&
    ast.fallback.type === "value"
  ) {
    return {
      type: ast.expression.as,
      fallback: stringifyFallbackValue(ast.fallback.value),
      enabled: conversion.selected ?? true,
      supported: true,
    };
  }

  return {
    type: normalizeType(conversion.type),
    fallback: "",
    enabled: conversion.selected ?? true,
    supported: false,
  };
}

export interface ColumnConversionFormProps {
  /** The column to configure conversion for */
  column: Column;
  /** Callback when a new conversion is created */
  onCreate?: (payload: ColumnConversionCreatePayload) => Promise<void>;
  /** Callback when an existing conversion is updated */
  onUpdate?: (
    conversionId: string,
    payload: ColumnConversionUpdatePayload,
  ) => Promise<void>;
  /** Callback when a conversion is deleted */
  onDelete?: (conversionId: string) => Promise<void>;
  /** Callback when cancel/close is clicked */
  onClose: () => void;
  /** Whether any operation is in progress */
  loading?: boolean;
}

type FormValues = {
  type: LogicalCastType | "";
  fallback: string;
  enabled: boolean;
};

export function ColumnConversionForm({
  column,
  onCreate,
  onUpdate,
  onDelete,
  onClose,
  loading = false,
}: ColumnConversionFormProps) {
  const derived = useMemo(() => deriveConversionState(column), [column]);
  const [error, setError] = useState<string | null>(null);

  const form = useForm<FormValues>({
    initialValues: {
      type: derived.type,
      fallback: derived.fallback,
      enabled: derived.enabled,
    },
    validate: {
      type: (value: LogicalCastType | "") =>
        !value ? "Select a target type" : null,
    },
  });

  const handleSubmit = async (values: FormValues) => {
    setError(null);
    if (!values.type) {
      form.setFieldError("type", "Select a target type");
      return;
    }

    const ast = buildCastAst(column.name, values.type, values.fallback);

    try {
      if (column.conversion && onUpdate) {
        await onUpdate(column.conversion.id, {
          ast,
          type: values.type,
          selected: values.enabled,
        });
      } else if (onCreate) {
        await onCreate({
          columnId: column.id,
          ast,
          type: values.type,
          selected: values.enabled,
        });
      }
    } catch (err) {
      setError(err instanceof Error ? err.message : "Operation failed");
    }
  };

  const handleDelete = async () => {
    if (!column.conversion || !onDelete) return;
    setError(null);
    try {
      await onDelete(column.conversion.id);
    } catch (err) {
      setError(err instanceof Error ? err.message : "Delete failed");
    }
  };

  return (
    <Stack gap="sm">
      {!derived.supported && (
        <Alert
          color="yellow"
          icon={<IconInfoCircle size={16} />}
          title="Editing will replace the existing conversion"
        >
          The current conversion uses an advanced expression. Saving will
          overwrite it with a simple cast/coalesce.
        </Alert>
      )}
      <Box>
        <Group gap="xs" align="center" mb={4}>
          <Text size="sm" fw={500}>
            Target type
          </Text>
          <HoverCard width={320} shadow="md" withArrow>
            <HoverCard.Target>
              <IconInfoCircle size={14} style={{ cursor: "pointer" }} />
            </HoverCard.Target>
            <HoverCard.Dropdown>
              <Text size="xs" fw={600} mb={4}>
                When to use each
              </Text>
              <List spacing={6} size="xs">
                <List.Item>
                  <strong>Number</strong>: generic numeric; maps to the
                  connector&apos;s default numeric type.
                </List.Item>
                <List.Item>
                  <strong>Decimal</strong>: fixed precision/scale (e.g.
                  NUMERIC(38,6)); prefer for currency and totals.
                </List.Item>
                <List.Item>
                  <strong>Integer</strong>: whole numbers (counts, flags).
                </List.Item>
                <List.Item>
                  <strong>BigInt</strong>: very large whole numbers (IDs that
                  exceed 32-bit).
                </List.Item>
                <List.Item>
                  <strong>Float</strong>: approximate decimals; use when you
                  need range/speed over precision.
                </List.Item>
              </List>
            </HoverCard.Dropdown>
          </HoverCard>
        </Group>
        <Select
          data={logicalCastTypeOptions}
          value={form.values.type}
          onChange={(value) =>
            form.setFieldValue(
              "type",
              value && logicalCastTypeOptions.some((opt) => opt.value === value)
                ? (value as LogicalCastType)
                : "",
            )
          }
          placeholder="Select target type"
          withCheckIcon={false}
          error={form.errors.type}
          disabled={loading}
        />
      </Box>
      <TextInput
        label="Fallback value (optional)"
        description='Used when casting fails; accepts number, boolean, or string. Leave blank to keep the default (no fallback/null). Typing "null" is the same as leaving this blank.'
        value={form.values.fallback}
        onChange={(event) => form.setFieldValue("fallback", event.target.value)}
        disabled={loading}
      />
      <Switch
        label="Enable conversion"
        checked={form.values.enabled}
        onChange={(event) =>
          form.setFieldValue("enabled", event.currentTarget.checked)
        }
        disabled={loading}
      />
      {error && (
        <Alert color="red" icon={<IconInfoCircle size={16} />}>
          {error}
        </Alert>
      )}
      <Group justify="space-between" mt="xs">
        <Group>
          {column.conversion && onDelete && (
            <Button
              color="red"
              variant="light"
              onClick={handleDelete}
              loading={loading}
            >
              Delete conversion
            </Button>
          )}
        </Group>
        <Group>
          <Button variant="default" onClick={onClose} disabled={loading}>
            Cancel
          </Button>
          <Button
            onClick={() => void handleSubmit(form.values)}
            loading={loading}
          >
            Save conversion
          </Button>
        </Group>
      </Group>
    </Stack>
  );
}
