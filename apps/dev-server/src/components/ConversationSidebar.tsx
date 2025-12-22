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
import { IconPlus, IconTrash, IconBraces } from "@tabler/icons-react";

export interface ConversationItem {
  id: string;
  title: string | null;
  createdAt: string;
  updatedAt: string;
  requestContext?: Record<string, string | number> | null;
}

interface ConversationSidebarProps {
  conversations: ConversationItem[];
  activeId?: string;
  onSelect: (id: string) => void;
  onNew: () => void;
  onDelete: (id: string) => void;
}

export function ConversationSidebar({
  conversations,
  activeId,
  onSelect,
  onNew,
  onDelete,
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
                onClick={() => onSelect(conv.id)}
                style={{
                  cursor: "pointer",
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
                    <Group gap={4} mb={2}>
                      <Text size="xs" c="dimmed">
                        {new Date(conv.updatedAt).toLocaleDateString()}
                      </Text>
                      {conv.requestContext &&
                        Object.keys(conv.requestContext).length > 0 && (
                          <Badge
                            size="xs"
                            variant="light"
                            color="blue"
                            leftSection={<IconBraces size={10} />}
                          >
                            {Object.keys(conv.requestContext).length}
                          </Badge>
                        )}
                    </Group>
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
