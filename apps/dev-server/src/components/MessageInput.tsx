"use client";

import { useState, useRef, useEffect } from "react";
import { TextInput, ActionIcon, Group, Box } from "@mantine/core";
import { IconSend2, IconSearch } from "@tabler/icons-react";

interface MessageInputProps {
  onSend: (message: string) => void;
  disabled?: boolean;
  placeholder?: string;
}

export function MessageInput({
  onSend,
  disabled = false,
  placeholder = "Ask about your data...",
}: MessageInputProps) {
  const [message, setMessage] = useState("");
  const inputRef = useRef<HTMLInputElement>(null);

  const handleSubmit = () => {
    const trimmed = message.trim();
    if (trimmed && !disabled) {
      onSend(trimmed);
      setMessage("");
    }
  };

  const handleKeyDown = (e: React.KeyboardEvent<HTMLInputElement>) => {
    if (e.key === "Enter" && !e.shiftKey) {
      e.preventDefault();
      handleSubmit();
    }
  };

  useEffect(() => {
    inputRef.current?.focus();
  }, []);

  return (
    <Box p="md" bg="gray.1">
      <Group gap="sm" align="center">
        <TextInput
          ref={inputRef}
          value={message}
          onChange={(e) => setMessage(e.currentTarget.value)}
          onKeyDown={handleKeyDown}
          placeholder={placeholder}
          leftSection={<IconSearch size={16} />}
          style={{ flex: 1 }}
          size="md"
        />
        <ActionIcon
          size="lg"
          variant="filled"
          color="dark"
          onClick={handleSubmit}
          disabled={disabled || !message.trim()}
        >
          <IconSend2 size={18} />
        </ActionIcon>
      </Group>
    </Box>
  );
}
