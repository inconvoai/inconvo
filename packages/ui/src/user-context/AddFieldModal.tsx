"use client";

import { useState } from "react";
import { Modal, Stack, TextInput, Select, Group, Button } from "@mantine/core";

export interface AddFieldModalProps {
  /** Whether the modal is open */
  opened: boolean;
  /** Callback when the modal is closed */
  onClose: () => void;
  /** Callback when a field is submitted */
  onSubmit: (field: {
    key: string;
    type: "STRING" | "NUMBER" | "BOOLEAN";
  }) => void;
  /** Whether the submit button should show loading state */
  loading?: boolean;
  /** Modal title (defaults to "Add Context Field") */
  title?: string;
  /** List of existing field keys (for duplicate validation) */
  existingKeys?: string[];
}

export function AddFieldModal({
  opened,
  onClose,
  onSubmit,
  loading = false,
  title = "Add Context Field",
  existingKeys = [],
}: AddFieldModalProps) {
  const [key, setKey] = useState("");
  const [type, setType] = useState<string | null>("STRING");

  const isDuplicate = existingKeys.includes(key.trim());
  const keyError = isDuplicate ? "A field with this key already exists" : null;

  const handleSubmit = () => {
    if (key && type && !isDuplicate) {
      onSubmit({
        key: key.trim(),
        type: type as "STRING" | "NUMBER" | "BOOLEAN",
      });
      // Reset form
      setKey("");
      setType("STRING");
    }
  };

  const handleClose = () => {
    // Reset form on close
    setKey("");
    setType("STRING");
    onClose();
  };

  return (
    <Modal opened={opened} onClose={handleClose} title={title}>
      <Stack gap="md">
        <TextInput
          label="Field Key"
          description="The key used to pass context values at runtime"
          placeholder="e.g., user_id, tenant_id, organization_id"
          value={key}
          onChange={(e) => setKey(e.currentTarget.value)}
          error={keyError}
        />
        <Select
          label="Type"
          description="The data type of the context value"
          data={[
            { value: "STRING", label: "String" },
            { value: "NUMBER", label: "Number" },
            { value: "BOOLEAN", label: "Boolean" },
          ]}
          value={type}
          onChange={setType}
        />
        <Group justify="flex-end" gap="sm">
          <Button variant="subtle" onClick={handleClose}>
            Cancel
          </Button>
          <Button
            onClick={handleSubmit}
            disabled={!key || !type || isDuplicate}
            loading={loading}
          >
            Add Field
          </Button>
        </Group>
      </Stack>
    </Modal>
  );
}
