"use client";

import { useState, useEffect, useCallback } from "react";
import {
  Container,
  Title,
  Text,
  Stack,
  Loader,
  Center,
  Alert,
  Group,
  Button,
  SegmentedControl,
  List,
  Paper,
  Divider,
} from "@mantine/core";
import { IconBraces, IconAlertCircle, IconCheck } from "@tabler/icons-react";
import {
  ContextFieldsTable,
  TableConditionsTable,
  AddFieldModal,
  AddConditionModal,
  type ContextField,
  type TableCondition,
  type TableInfo,
} from "@repo/ui/user-context";
import posthog from "posthog-js";
import { trackFeatureUsageClient } from "~/lib/telemetry";

// API response types (slightly different from component types)
interface ApiUserContextField {
  id: string;
  key: string;
  type: string;
  tableConditions: Array<{
    id: string;
    table: { id: string; name: string };
    column: { id: string; name: string };
  }>;
}

interface ApiTableCondition {
  id: string;
  table: { id: string; name: string };
  column: { id: string; name: string };
  userContextField: { id: string; key: string };
}

// Transform API fields to component format
function toContextFields(apiFields: ApiUserContextField[]): ContextField[] {
  return apiFields.map((f) => ({
    id: f.id,
    key: f.key,
    type: f.type as "STRING" | "NUMBER" | "BOOLEAN",
    tableConditions: f.tableConditions.map((tc) => ({
      id: tc.id,
      tableName: tc.table.name,
      columnName: tc.column.name,
    })),
  }));
}

export default function UserContextPage() {
  const [apiFields, setApiFields] = useState<ApiUserContextField[]>([]);
  const [conditions, setConditions] = useState<ApiTableCondition[]>([]);
  const [tables, setTables] = useState<TableInfo[]>([]);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);
  const [successMessage, setSuccessMessage] = useState<string | null>(null);
  const [contextStatus, setContextStatus] = useState<
    "UNSET" | "ENABLED" | "DISABLED"
  >("UNSET");
  const [statusUpdating, setStatusUpdating] = useState(false);
  const isDisabled = contextStatus === "DISABLED";

  const [showAddField, setShowAddField] = useState(false);
  const [showAddCondition, setShowAddCondition] = useState(false);
  const [addFieldLoading, setAddFieldLoading] = useState(false);
  const [addConditionLoading, setAddConditionLoading] = useState(false);

  const fetchData = useCallback(async () => {
    try {
      const [fieldsRes, conditionsRes, tablesRes] = await Promise.all([
        fetch("/api/schema/user-context"),
        fetch("/api/schema/table-conditions"),
        fetch("/api/schema/tables"),
      ]);

      const [fieldsData, conditionsData, tablesData] = (await Promise.all([
        fieldsRes.json(),
        conditionsRes.json(),
        tablesRes.json(),
      ])) as [
        {
          error?: string;
          fields?: ApiUserContextField[];
          status?: "UNSET" | "ENABLED" | "DISABLED";
        },
        { error?: string; conditions?: ApiTableCondition[] },
        { error?: string; tables?: Array<{ name: string }> },
      ];

      if (fieldsData.error) throw new Error(fieldsData.error);
      if (conditionsData.error) throw new Error(conditionsData.error);

      setApiFields(fieldsData.fields ?? []);
      setContextStatus(fieldsData.status ?? "UNSET");
      setConditions(conditionsData.conditions ?? []);

      // Fetch full table details for column selection
      const tableDetails: TableInfo[] = [];
      for (const table of tablesData.tables ?? []) {
        const detailRes = await fetch(
          `/api/schema/tables/${encodeURIComponent(table.name)}`,
        );
        const detailData = (await detailRes.json()) as {
          table?: { id: string; name: string; columns: TableInfo["columns"] };
        };
        if (detailData.table) {
          tableDetails.push({
            id: detailData.table.id,
            name: detailData.table.name,
            columns: detailData.table.columns,
          });
        }
      }
      setTables(tableDetails);
    } catch (err) {
      setError(err instanceof Error ? err.message : "Failed to fetch data");
    } finally {
      setLoading(false);
    }
  }, []);

  const updateStatus = async (status: "ENABLED" | "DISABLED") => {
    try {
      setStatusUpdating(true);
      const res = await fetch("/api/schema/user-context", {
        method: "PATCH",
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify({ status }),
      });
      const data = (await res.json()) as { status?: string; error?: string };
      if (!res.ok) {
        throw new Error(data.error ?? "Failed to update status");
      }
      setContextStatus(status);
      setSuccessMessage("User context status updated");
    } catch (err) {
      setError(err instanceof Error ? err.message : "Failed to update status");
    } finally {
      setStatusUpdating(false);
    }
  };

  useEffect(() => {
    void fetchData();
  }, [fetchData]);

  const handleAddField = async (field: {
    key: string;
    type: "STRING" | "NUMBER" | "BOOLEAN";
  }) => {
    setAddFieldLoading(true);
    try {
      const res = await fetch("/api/schema/user-context", {
        method: "POST",
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify(field),
      });
      const data = (await res.json()) as { error?: string };
      if (data.error) throw new Error(data.error);

      // Track context field creation
      trackFeatureUsageClient(posthog, "user_context", { action: "context_field_created" });

      setSuccessMessage("Request context field created");
      setShowAddField(false);
      await fetchData();
      setTimeout(() => setSuccessMessage(null), 3000);
    } catch (err) {
      setError(err instanceof Error ? err.message : "Failed to create field");
    } finally {
      setAddFieldLoading(false);
    }
  };

  const handleDeleteField = async (id: string) => {
    try {
      const res = await fetch(`/api/schema/user-context/${id}`, {
        method: "DELETE",
      });
      const data = (await res.json()) as { error?: string };
      if (data.error) throw new Error(data.error);

      // Track context field deletion
      trackFeatureUsageClient(posthog, "user_context", { action: "context_field_deleted" });

      setSuccessMessage("Request context field deleted");
      await fetchData();
      setTimeout(() => setSuccessMessage(null), 3000);
    } catch (err) {
      setError(err instanceof Error ? err.message : "Failed to delete field");
    }
  };

  const handleAddCondition = async (condition: {
    tableId: string;
    columnId: string;
    userContextFieldId: string;
  }) => {
    setAddConditionLoading(true);
    try {
      const res = await fetch("/api/schema/table-conditions", {
        method: "POST",
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify(condition),
      });
      const data = (await res.json()) as { error?: string };
      if (data.error) throw new Error(data.error);

      // Track table condition creation
      trackFeatureUsageClient(posthog, "user_context", { action: "table_condition_created" });

      setSuccessMessage("Table condition created");
      setShowAddCondition(false);
      await fetchData();
      setTimeout(() => setSuccessMessage(null), 3000);
    } catch (err) {
      setError(
        err instanceof Error ? err.message : "Failed to create condition",
      );
    } finally {
      setAddConditionLoading(false);
    }
  };

  const handleDeleteCondition = async (id: string) => {
    try {
      const res = await fetch(`/api/schema/table-conditions/${id}`, {
        method: "DELETE",
      });
      const data = (await res.json()) as { error?: string };
      if (data.error) throw new Error(data.error);

      // Track table condition deletion
      trackFeatureUsageClient(posthog, "user_context", { action: "table_condition_deleted" });

      setSuccessMessage("Table condition deleted");
      await fetchData();
      setTimeout(() => setSuccessMessage(null), 3000);
    } catch (err) {
      setError(
        err instanceof Error ? err.message : "Failed to delete condition",
      );
    }
  };

  // Transform API types to component types
  const fields = toContextFields(apiFields);
  const tableConditions: TableCondition[] = conditions.map((c) => ({
    id: c.id,
    table: c.table,
    column: c.column,
    userContextField: c.userContextField,
  }));
  const statusOptions = [
    { label: "Enabled", value: "ENABLED" },
    { label: "Disabled", value: "DISABLED" },
  ];
  const handleStatusChange = (value: string) => {
    if (value === "UNSET") {
      return;
    }
    void updateStatus(value as "ENABLED" | "DISABLED");
  };

  if (loading) {
    return (
      <Container size="lg" py="xl">
        <Center>
          <Loader size="lg" />
        </Center>
      </Container>
    );
  }

  return (
    <Container size="lg" py="md">
      <Stack gap="lg">
        <Group justify="space-between" align="center">
          <Group gap="sm">
            <IconBraces size={28} />
            <Title order={2}>User Context</Title>
          </Group>
          {contextStatus !== "UNSET" && (
            <SegmentedControl
              value={contextStatus}
              onChange={handleStatusChange}
              data={statusOptions}
              disabled={statusUpdating}
            />
          )}
        </Group>

        <Paper withBorder p="md">
          <Stack gap="xs">
            <Text size="sm" fw={600}>
              How it works
            </Text>
            <Text size="sm" c="dimmed">
              User Context defines a schema of key-value fields stored on each
              conversation. Values are provided when a conversation is created
              and reused across all requests. When enabled, conversations must
              include values that match this schema or creation will fail.
            </Text>
            <List size="sm" c="dimmed" spacing={2}>
              <List.Item>
                Passed to the model for user-specific behavior.
              </List.Item>
              <List.Item>
                Used for access constraints on tables when you add constraints.
              </List.Item>
              <List.Item>
                Used to scope datasets when you upload datasets for users who
                share specific context values.
              </List.Item>
            </List>
          </Stack>
        </Paper>

        {contextStatus === "UNSET" && (
          <Alert icon={<IconAlertCircle size={16} />} color="gray" variant="light">
            <Text size="sm">
              Note: User context must be enabled to set per-table row-level
              access constraints in the semantic model (multi-tenant databases).
            </Text>
          </Alert>
        )}

        {contextStatus === "UNSET" && (
          <Center py="xl" mih={220}>
            <Stack gap="xs" align="center">
              <Text size="sm" fw={600}>
                Do you want to enable User Context for this agent?
              </Text>
              <Group gap="sm">
                <Button
                  size="sm"
                  onClick={() => handleStatusChange("ENABLED")}
                  disabled={statusUpdating}
                >
                  Yes, enable
                </Button>
                <Button
                  size="sm"
                  variant="light"
                  color="gray"
                  onClick={() => handleStatusChange("DISABLED")}
                  disabled={statusUpdating}
                >
                  No, disable
                </Button>
              </Group>
            </Stack>
          </Center>
        )}

        {isDisabled && (
          <Alert icon={<IconAlertCircle size={16} />} color="gray" variant="light">
            <Text size="sm">
              User context is disabled. Conversations ignore context values,
              and access constraints and dataset uploads do not apply.
            </Text>
          </Alert>
        )}

        {contextStatus !== "UNSET" && <Divider />}

        {error && (
          <Alert
            icon={<IconAlertCircle size={16} />}
            color="red"
            onClose={() => setError(null)}
            withCloseButton
          >
            {error}
          </Alert>
        )}

        {successMessage && (
          <Alert
            icon={<IconCheck size={16} />}
            color="green"
            onClose={() => setSuccessMessage(null)}
            withCloseButton
          >
            {successMessage}
          </Alert>
        )}

        {contextStatus === "ENABLED" && (
          <>
            <ContextFieldsTable
              fields={fields}
              onAddClick={() => setShowAddField(true)}
              onDeleteField={handleDeleteField}
            />

            <TableConditionsTable
              conditions={tableConditions}
              onAddClick={() => setShowAddCondition(true)}
              onDeleteCondition={handleDeleteCondition}
              disabled={fields.length === 0 || tables.length === 0}
            />

            <AddFieldModal
              opened={showAddField}
              onClose={() => setShowAddField(false)}
              onSubmit={handleAddField}
              loading={addFieldLoading}
            />

            <AddConditionModal
              opened={showAddCondition}
              onClose={() => setShowAddCondition(false)}
              onSubmit={handleAddCondition}
              connections={[{ id: "default", name: "Local Database" }]}
              tables={tables}
              fields={fields}
              loading={addConditionLoading}
            />
          </>
        )}
      </Stack>
    </Container>
  );
}
