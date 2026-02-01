"use client";

import { Text, Paper, Flex, Avatar } from "@mantine/core";
import { IconUser } from "@tabler/icons-react";
import dynamic from "next/dynamic";
import { MessageContent } from "@repo/ui/chat";
import type { InconvoResponse, InconvoMessage } from "@repo/types";
import { Logo } from "@repo/ui";

const VegaChart = dynamic(
  () => import("@repo/ui/chat/VegaChart").then((mod) => mod.VegaChart),
  { ssr: false }
);

interface MessageProps {
  message: InconvoResponse | { type: "user"; message: string };
  isUser?: boolean;
  isStreaming?: boolean;
}

export function Message({
  message,
  isUser = false,
  isStreaming = false,
}: MessageProps) {
  const renderContent = () => {
    if (message.type === "user") {
      return (
        <Text style={{ whiteSpace: "pre-wrap" }} c="black">
          {message.message}
        </Text>
      );
    }
    return (
      <MessageContent
        message={message as InconvoMessage}
        isStreaming={isStreaming}
        renderVegaChart={(spec) => <VegaChart spec={spec} />}
      />
    );
  };

  return (
    <Flex justify={isUser ? "flex-end" : "flex-start"} gap="sm" mb="md">
      {!isUser && (
        <Flex justify="flex-start" align="flex-start" wrap="nowrap" gap="sm">
          <Logo size="md" />
          <Paper withBorder p="xs" px="sm" radius="md" bg="white">
            {renderContent()}
          </Paper>
        </Flex>
      )}
      {isUser && (
        <>
          <Paper withBorder p="xs" px="sm" radius="md" bg="gray.1">
            {renderContent()}
          </Paper>
          <Avatar>
            <IconUser />
          </Avatar>
        </>
      )}
    </Flex>
  );
}
