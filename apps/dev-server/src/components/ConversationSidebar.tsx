"use client";

import {
  Box,
  Stack,
  Text,
  ActionIcon,
  Group,
  Card,
  ScrollArea,
  Badge,
} from "@mantine/core";
import { IconPlus, IconTrash } from "@tabler/icons-react";

export interface ConversationItem {
  id: string;
  title: string | null;
  createdAt: string;
  updatedAt: string;
  userContext?: Record<string, string | number | boolean> | null;
  userIdentifier?: string | null;
}

interface ConversationSidebarProps {
  conversations: ConversationItem[];
  activeId?: string;
  onSelect: (id: string) => void;
  onNew: () => void;
  onDelete: (id: string) => void;
  disabled?: boolean;
}

export function ConversationSidebar({
  conversations,
  activeId,
  onSelect,
  onNew,
  onDelete,
  disabled = false,
}: ConversationSidebarProps) {
  return (
    <Box
      w={300}
      bg="gray.0"
      style={{
        borderRight: "1px solid var(--mantine-color-gray-3)",
        display: "flex",
        flexDirection: "column",
        height: "100%",
      }}
    >
      <Group p="md" justify="space-between">
        <Text fw={600} size="sm">
          Conversations
        </Text>
        <ActionIcon
          variant="light"
          color="blue"
          onClick={onNew}
          disabled={disabled}
          title="New conversation"
        >
          <IconPlus size={16} />
        </ActionIcon>
      </Group>

      <ScrollArea style={{ flex: 1 }} px="sm">
        <Stack gap={4}>
          {conversations.length === 0 ? (
            <Text size="xs" c="dimmed" ta="center" py="xl">
              No conversations yet
            </Text>
          ) : (
            conversations.map((conv) => (
              <Card
                key={conv.id}
                padding="xs"
                radius="md"
                onClick={() => !disabled && onSelect(conv.id)}
                style={{
                  cursor: disabled ? "not-allowed" : "pointer",
                  opacity: disabled ? 0.6 : 1,
                  backgroundColor:
                    activeId === conv.id
                      ? "var(--mantine-color-blue-0)"
                      : "transparent",
                  border:
                    activeId === conv.id
                      ? "1px solid var(--mantine-color-blue-2)"
                      : "1px solid transparent",
                }}
              >
                <Group justify="space-between" wrap="nowrap">
                  <Box style={{ flex: 1, minWidth: 0 }}>
                    <Group gap={8} mb={4}>
                      <Text size="xs" c="dimmed">
                        {new Date(conv.updatedAt).toLocaleDateString()}
                      </Text>
                      {conv.userIdentifier && (
                        <Badge size="xs" variant="dot" color="gray">
                          {conv.userIdentifier}
                        </Badge>
                      )}
                    </Group>
                    {conv.userContext &&
                      Object.keys(conv.userContext).length > 0 && (
                        <ScrollArea
                          type="never"
                          mb={4}
                          style={{ width: "100%" }}
                        >
                          <Group gap={4} wrap="nowrap">
                            {Object.entries(conv.userContext).map(
                              ([key, value]) => (
                                <Badge
                                  key={key}
                                  size="xs"
                                  variant="light"
                                  color="blue"
                                  style={{ flexShrink: 0 }}
                                >
                                  {key}: {value}
                                </Badge>
                              ),
                            )}
                          </Group>
                        </ScrollArea>
                      )}
                    <Text size="sm" truncate>
                      {conv.title ?? "New conversation"}
                    </Text>
                  </Box>
                  <ActionIcon
                    size="sm"
                    variant="subtle"
                    color="red"
                    onClick={(e) => {
                      e.stopPropagation();
                      onDelete(conv.id);
                    }}
                  >
                    <IconTrash size={14} />
                  </ActionIcon>
                </Group>
              </Card>
            ))
          )}
        </Stack>
      </ScrollArea>
    </Box>
  );
}
