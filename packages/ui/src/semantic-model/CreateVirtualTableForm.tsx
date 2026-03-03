"use client";

import { useState, useCallback, useMemo } from "react";
import {
  Alert,
  Badge,
  Box,
  Button,
  Group,
  Select,
  Stack,
  Text,
  TextInput,
} from "@mantine/core";
import Editor, { loader } from "@monaco-editor/react";
import * as monaco from "monaco-editor";
import { useSqlEditor } from "./useSqlEditor";
import type {
  TableAccess,
  TableWithColumns,
  VirtualTableDialect,
  VirtualTableValidationResult,
} from "./types";

if (typeof window !== "undefined") {
  loader.config({ monaco });
}

interface CreateVirtualTableFormProps {
  connectionId: string;
  connectionName: string;
  availableTables: TableWithColumns[];
  dialect?: VirtualTableDialect;
  onValidateSql: (payload: {
    connectionId: string;
    sql: string;
    previewLimit?: number;
  }) => Promise<VirtualTableValidationResult>;
  onCreate: (payload: {
    connectionId: string;
    name: string;
    sql: string;
    access: TableAccess;
  }) => Promise<void>;
  onClose: () => void;
  disabled?: boolean;
}

const ACCESS_OPTIONS: { value: string; label: string }[] = [
  { value: "QUERYABLE", label: "Queryable" },
  { value: "JOINABLE", label: "Joinable" },
  { value: "OFF", label: "Off" },
];

function formatErrorMessage(error: unknown): string {
  if (error instanceof Error) {
    return error.message;
  }
  return "Something went wrong.";
}

export function CreateVirtualTableForm({
  connectionId,
  connectionName,
  availableTables,
  dialect,
  onValidateSql,
  onCreate,
  onClose,
  disabled = false,
}: CreateVirtualTableFormProps) {
  const [name, setName] = useState("");
  const [sql, setSql] = useState("");
  const [access, setAccess] = useState<TableAccess>("OFF");
  const [error, setError] = useState<string | null>(null);
  const [creating, setCreating] = useState(false);
  const isMac = useMemo(
    () => typeof navigator !== "undefined" && /mac/i.test(navigator.userAgent),
    [],
  );

  const canSubmit = useMemo(
    () => name.trim().length > 0 && sql.trim().length > 0,
    [name, sql],
  );

  const handleCreate = useCallback(async () => {
    if (disabled || creating) {
      return;
    }
    if (!name.trim()) {
      setError("Name is required.");
      return;
    }
    if (!sql.trim()) {
      setError("SQL is required.");
      return;
    }
    setError(null);
    setCreating(true);
    try {
      const validationResult: VirtualTableValidationResult = await onValidateSql({
        connectionId,
        sql,
        previewLimit: 5,
      });
      if (!validationResult.ok) {
        setError(validationResult.error.message);
        return;
      }
      if (validationResult.columns.length === 0) {
        setError(
          "Validation returned no inferred columns. Ensure the query returns at least one row.",
        );
        return;
      }
      await onCreate({ connectionId, name: name.trim(), sql, access });
    } catch (err) {
      setError(formatErrorMessage(err));
    } finally {
      setCreating(false);
    }
  }, [access, connectionId, creating, disabled, name, onCreate, onValidateSql, sql]);

  const { beforeMount: handleEditorBeforeMount, onMount: handleEditorMount } = useSqlEditor({
    dialect,
    availableTables,
    onSave: () => {
      void handleCreate();
    },
  });

  return (
    <Stack gap="sm" p="md">
      <Group justify="space-between" align="flex-start">
        <div>
          <Text fw={600}>Create Virtual Table</Text>
          <Text size="sm" c="dimmed">
            Define a read-only SQL query for this connection. It will appear in
            the semantic model as a normal table with a virtual source.
          </Text>
        </div>
        <Button variant="subtle" size="xs" onClick={onClose} disabled={creating}>
          Cancel
        </Button>
      </Group>

      <Group grow align="flex-start">
        <TextInput
          label="Name"
          placeholder="reservations_by_segment"
          value={name}
          onChange={(e) => {
            setName(e.currentTarget.value);
            setError(null);
          }}
          disabled={disabled || creating}
        />
        <Select
          label="Access"
          data={ACCESS_OPTIONS}
          value={access}
          onChange={(value) => {
            setAccess((value as TableAccess) ?? "OFF");
            setError(null);
          }}
          disabled={disabled || creating}
        />
      </Group>
      <Stack gap={4}>
        <Text size="sm" fw={500}>
          SQL
        </Text>
        <Box
          style={{
            border: "1px solid var(--mantine-color-default-border)",
            borderRadius: "var(--mantine-radius-sm)",
            overflow: "hidden",
          }}
        >
          <Editor
            height="220px"
            defaultLanguage="sql"
            theme="sqlEditorTheme"
            beforeMount={handleEditorBeforeMount}
            onMount={handleEditorMount}
            value={sql}
            onChange={(value) => {
              setSql(value ?? "");
              setError(null);
            }}
            options={{
              minimap: { enabled: false },
              fontSize: 14,
              scrollBeyondLastLine: false,
              wordWrap: "on",
              lineNumbers: "on",
              glyphMargin: false,
              folding: false,
              lineDecorationsWidth: 8,
              lineNumbersMinChars: 3,
              renderLineHighlight: "none",
              scrollbar: {
                vertical: "auto",
                horizontal: "hidden",
              },
              readOnly: disabled || creating,
              padding: { top: 8, bottom: 8 },
            }}
          />
        </Box>
      </Stack>

      <Group gap={6} wrap="wrap">
        <Badge variant="light" color="gray">
          Connection
        </Badge>
        <Text size="sm">{connectionName}</Text>
      </Group>

      {error ? <Alert color="red">{error}</Alert> : null}

      <Group justify="space-between" wrap="wrap">
        <Text size="xs" c="dimmed">
          Columns are created as normal semantic-model columns and can be renamed
          later.
        </Text>
        <Group gap="xs">
          <Button
            onClick={handleCreate}
            loading={creating}
            disabled={disabled || !canSubmit}
          >
            Create Virtual Table{" "}
            {!creating ? (
              <Text span size="xs" c="dimmed" ml={4}>
                {isMac ? "Cmd+Enter" : "Ctrl+Enter"}
              </Text>
            ) : null}
          </Button>
        </Group>
      </Group>
    </Stack>
  );
}
