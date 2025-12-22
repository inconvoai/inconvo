"use client";

import { useRef, useEffect } from "react";
import { Box, Stack, Text, Loader, Center, Flex } from "@mantine/core";
import { Message } from "./Message";
import type { InconvoResponse } from "@repo/types";

export interface ChatMessage {
  id: string;
  type: "user" | InconvoResponse["type"];
  content: string | InconvoResponse;
}

interface MessageListProps {
  messages: ChatMessage[];
  isLoading?: boolean;
  progressMessage?: string;
}

export function MessageList({
  messages,
  isLoading = false,
  progressMessage,
}: MessageListProps) {
  const bottomRef = useRef<HTMLDivElement>(null);

  useEffect(() => {
    bottomRef.current?.scrollIntoView({ behavior: "smooth" });
  }, [messages, progressMessage]);

  if (messages.length === 0 && !isLoading) {
    return (
      <Center style={{ flex: 1 }}>
        <Stack align="center" gap="sm">
          <Text size="xl" fw={500} c="dimmed">
            Start a conversation
          </Text>
          <Text size="sm" c="dimmed">
            Ask questions about your data
          </Text>
        </Stack>
      </Center>
    );
  }

  return (
    <Box
      style={{
        flex: 1,
        overflow: "auto",
        padding: "var(--mantine-spacing-md)",
      }}
      bg="white"
    >
      <Stack gap="xs">
        {messages.map((msg) => {
          if (msg.type === "user") {
            return (
              <Message
                key={msg.id}
                message={{ type: "user", message: msg.content as string }}
                isUser
              />
            );
          }
          return (
            <Message
              key={msg.id}
              message={msg.content as InconvoResponse}
              isUser={false}
            />
          );
        })}
        {isLoading && (
          <Flex align="center" gap="sm" p="md">
            <Loader size="sm" type="dots" />
            <Text size="sm" c="dimmed">
              {progressMessage ?? "Thinking..."}
            </Text>
          </Flex>
        )}
        <div ref={bottomRef} />
      </Stack>
    </Box>
  );
}
