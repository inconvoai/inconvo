"use client";

import { useState, useCallback, useEffect } from "react";
import {
  Box,
  Alert,
  Flex,
  Paper,
  Text,
  Group,
  Badge,
  TextInput,
  ActionIcon,
  Collapse,
  Stack,
  Button,
  Center,
  Title,
  ThemeIcon,
} from "@mantine/core";
import {
  IconAlertCircle,
  IconBraces,
  IconChevronDown,
  IconChevronUp,
  IconCheck,
} from "@tabler/icons-react";
import {
  ConversationSidebar,
  type ConversationItem,
} from "./ConversationSidebar";
import { MessageList, type ChatMessage } from "./MessageList";
import { MessageInput } from "./MessageInput";
import type { InconvoResponse } from "@repo/types";

interface UserContextField {
  id: string;
  key: string;
  type: string;
}

interface StreamEvent {
  type:
    | "response.created"
    | "response.progress"
    | "response.completed"
    | "response.error";
  id: string;
  message?: string;
  response?: InconvoResponse;
  error?: string;
}

export function ChatInterface() {
  const [conversations, setConversations] = useState<ConversationItem[]>([]);
  const [activeConversationId, setActiveConversationId] = useState<
    string | undefined
  >();
  const [messages, setMessagesState] = useState<ChatMessage[]>([]);
  const [isLoading, setIsLoading] = useState(false);
  const [isCreatingConversation, setIsCreatingConversation] = useState(false);
  const [progressMessage, setProgressMessage] = useState<string>();
  const [error, setError] = useState<string>();

  // User context state
  const [contextFields, setContextFields] = useState<UserContextField[]>([]);
  const [contextValues, setContextValues] = useState<Record<string, string>>(
    {},
  );
  const [contextExpanded, setContextExpanded] = useState(false);
  const [contextConfirmed, setContextConfirmed] = useState(false);

  // Alias for setMessagesState (messages are now persisted server-side)
  const setMessages = setMessagesState;

  // Fetch conversations and context fields on mount
  useEffect(() => {
    void fetchConversations();
    void fetchContextFields();
  }, []);

  const fetchContextFields = async () => {
    try {
      const res = await fetch("/api/schema/user-context");
      const data = (await res.json()) as { fields?: UserContextField[] };
      setContextFields(data.fields ?? []);
    } catch (err) {
      console.error("Failed to fetch context fields:", err);
    }
  };

  const fetchConversations = async () => {
    try {
      const res = await fetch("/api/v1/agents/dev-agent/conversations");
      const data = (await res.json()) as { conversations?: ConversationItem[] };
      setConversations(data.conversations ?? []);
    } catch (err) {
      console.error("Failed to fetch conversations:", err);
    }
  };

  const handleNewConversation = useCallback(() => {
    setActiveConversationId(undefined);
    setMessagesState([]);
    setError(undefined);
    setContextValues({});
    setContextConfirmed(false);
  }, []);

  const handleSelectConversation = useCallback(async (id: string) => {
    setActiveConversationId(id);
    setError(undefined);

    // Load conversation with messages from server
    try {
      const res = await fetch(`/api/v1/agents/dev-agent/conversations/${id}`);
      if (res.ok) {
        const data = (await res.json()) as {
          userContext?: Record<string, string | number>;
          messages?: ChatMessage[];
        };
        // Load messages from server
        setMessagesState(data.messages ?? []);
        if (data.userContext) {
          // Convert context values to strings for the input fields
          const stringValues: Record<string, string> = {};
          for (const [key, value] of Object.entries(data.userContext)) {
            stringValues[key] = String(value);
          }
          setContextValues(stringValues);
        }
        setContextConfirmed(true);
      }
    } catch (err) {
      console.error("Failed to load conversation:", err);
    }
  }, []);

  const handleDeleteConversation = useCallback(
    async (id: string) => {
      try {
        await fetch(`/api/v1/agents/dev-agent/conversations/${id}`, {
          method: "DELETE",
        });
        await fetchConversations();
        if (activeConversationId === id) {
          handleNewConversation();
        }
      } catch (err) {
        console.error("Failed to delete conversation:", err);
      }
    },
    [activeConversationId, handleNewConversation],
  );

  // Build user context object from values, converting types as needed
  const buildUserContext = useCallback(() => {
    const context: Record<string, string | number> = {};
    for (const field of contextFields) {
      const value = contextValues[field.key];
      if (value !== undefined && value !== "") {
        if (field.type === "NUMBER") {
          const num = parseFloat(value);
          if (!isNaN(num)) {
            context[field.key] = num;
          }
        } else {
          context[field.key] = value;
        }
      }
    }
    return Object.keys(context).length > 0 ? context : undefined;
  }, [contextFields, contextValues]);

  // Create a new conversation with context
  const handleCreateConversation = useCallback(async () => {
    setIsCreatingConversation(true);
    setError(undefined);
    try {
      const userContext = buildUserContext();
      // Generate a unique user identifier (in production, this would come from auth)
      const userIdentifier = `dev-user-${Date.now()}`;

      const res = await fetch("/api/v1/agents/dev-agent/conversations", {
        method: "POST",
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify({ userIdentifier, userContext }),
      });

      if (!res.ok) {
        throw new Error(`Failed to create conversation: ${res.status}`);
      }

      const data = (await res.json()) as { id: string };
      setActiveConversationId(data.id);
      setContextConfirmed(true);
      await fetchConversations();
    } catch (err) {
      console.error("Failed to create conversation:", err);
      setError(
        err instanceof Error ? err.message : "Failed to create conversation",
      );
    } finally {
      setIsCreatingConversation(false);
    }
  }, [buildUserContext]);

  const handleSendMessage = useCallback(
    async (message: string) => {
      if (!activeConversationId) {
        setError("No active conversation");
        return;
      }

      setError(undefined);
      setIsLoading(true);
      setProgressMessage(undefined);

      // Add user message immediately
      const userMessageId = `user-${Date.now()}`;
      setMessages((prev) => [
        ...prev,
        { id: userMessageId, type: "user", content: message },
      ]);

      try {
        const response = await fetch(
          `/api/v1/agents/dev-agent/conversations/${activeConversationId}/response`,
          {
            method: "POST",
            headers: { "Content-Type": "application/json" },
            body: JSON.stringify({
              message,
              stream: true,
            }),
          },
        );

        if (!response.ok) {
          throw new Error(`HTTP ${response.status}`);
        }

        const reader = response.body?.getReader();
        if (!reader) throw new Error("No response body");

        const decoder = new TextDecoder();
        let buffer = "";

        while (true) {
          const { done, value } = await reader.read();
          if (done) break;

          buffer += decoder.decode(value, { stream: true });
          const lines = buffer.split("\n\n");
          buffer = lines.pop() ?? "";

          for (const line of lines) {
            if (line.startsWith("data: ")) {
              try {
                const event = JSON.parse(line.slice(6)) as StreamEvent;

                if (event.type === "response.progress" && event.message) {
                  setProgressMessage(event.message);
                }

                if (event.type === "response.completed" && event.response) {
                  setMessages((prev) => [
                    ...prev,
                    {
                      id: event.response!.id,
                      type: event.response!.type,
                      content: event.response!,
                    },
                  ]);
                  // Refresh conversation list to get updated titles
                  await fetchConversations();
                }

                if (event.type === "response.error") {
                  setError(event.error ?? "An error occurred");
                }
              } catch (parseErr) {
                console.error("Failed to parse SSE event:", parseErr);
              }
            }
          }
        }
      } catch (err) {
        console.error("Chat error:", err);
        setError(err instanceof Error ? err.message : "Failed to send message");
      } finally {
        setIsLoading(false);
        setProgressMessage(undefined);
      }
    },
    [activeConversationId],
  );

  // Count active context values
  const activeContextCount = Object.values(contextValues).filter(
    (v) => v !== undefined && v !== "",
  ).length;

  return (
    <Flex h="100%" gap={0}>
      <ConversationSidebar
        conversations={conversations}
        activeId={activeConversationId}
        onSelect={handleSelectConversation}
        onNew={handleNewConversation}
        onDelete={handleDeleteConversation}
      />

      <Box
        style={{
          flex: 1,
          display: "flex",
          flexDirection: "column",
          minWidth: 0,
        }}
        bg="white"
      >
        {error && (
          <Alert
            icon={<IconAlertCircle size={16} />}
            color="red"
            withCloseButton
            onClose={() => setError(undefined)}
            m="md"
          >
            {error}
          </Alert>
        )}

        {/* User Context Setup - shown before first message */}
        {contextFields.length > 0 &&
        messages.length === 0 &&
        !contextConfirmed ? (
          <Box
            style={{
              flex: 1,
              display: "flex",
              alignItems: "center",
              justifyContent: "center",
            }}
          >
            <Paper withBorder p="xl" maw={500} w="100%" m="md">
              <Stack gap="md">
                <Center>
                  <ThemeIcon size="xl" variant="light" color="blue">
                    <IconBraces size={24} />
                  </ThemeIcon>
                </Center>
                <Title order={4} ta="center">
                  Set User Context
                </Title>
                <Text size="sm" c="dimmed" ta="center">
                  Configure context values to filter queries by user identity,
                  tenant, or other criteria. These values will be used for
                  row-level security filtering.
                </Text>

                <Stack gap="sm">
                  {contextFields.map((field) => (
                    <TextInput
                      key={field.id}
                      label={field.key}
                      description={`Type: ${field.type}`}
                      placeholder={
                        field.type === "NUMBER"
                          ? "Enter a number"
                          : "Enter a value"
                      }
                      value={contextValues[field.key] ?? ""}
                      onChange={(e) => {
                        const value = e.currentTarget?.value ?? "";
                        setContextValues((prev) => ({
                          ...prev,
                          [field.key]: value,
                        }));
                      }}
                    />
                  ))}
                </Stack>

                <Button
                  fullWidth
                  leftSection={<IconCheck size={16} />}
                  onClick={handleCreateConversation}
                  disabled={activeContextCount === 0}
                  loading={isCreatingConversation}
                  mt="sm"
                >
                  Continue
                </Button>
              </Stack>
            </Paper>
          </Box>
        ) : (
          <>
            {/* User Context Display - show values once conversation has messages */}
            {contextFields.length > 0 &&
              messages.length > 0 &&
              activeContextCount > 0 && (
                <Paper withBorder m="md" mb={0} py="xs" px="md">
                  <Group gap="xs">
                    <IconBraces size={14} />
                    <Text size="sm" c="dimmed">
                      context:
                    </Text>
                    <Text size="sm" ff="monospace">
                      {JSON.stringify(buildUserContext())}
                    </Text>
                  </Group>
                </Paper>
              )}

            {/* Collapsible User Context Panel - only shown before first message */}
            {contextFields.length > 0 && messages.length === 0 && (
              <Paper withBorder m="md" mb={0} p="xs">
                <Group
                  justify="space-between"
                  style={{ cursor: "pointer" }}
                  onClick={() => setContextExpanded(!contextExpanded)}
                >
                  <Group gap="xs">
                    <IconBraces size={16} />
                    <Text size="sm" fw={500}>
                      User Context
                    </Text>
                    {activeContextCount > 0 && (
                      <Badge size="xs" variant="filled" color="blue">
                        {activeContextCount} set
                      </Badge>
                    )}
                  </Group>
                  <ActionIcon variant="subtle" size="sm">
                    {contextExpanded ? (
                      <IconChevronUp size={14} />
                    ) : (
                      <IconChevronDown size={14} />
                    )}
                  </ActionIcon>
                </Group>

                <Collapse in={contextExpanded}>
                  <Stack gap="xs" mt="sm">
                    <Text size="xs" c="dimmed">
                      Set context values to filter queries by user identity,
                      tenant, etc.
                    </Text>
                    {contextFields.map((field) => (
                      <TextInput
                        key={field.id}
                        label={field.key}
                        description={`Type: ${field.type}`}
                        placeholder={
                          field.type === "NUMBER"
                            ? "Enter a number"
                            : "Enter a value"
                        }
                        value={contextValues[field.key] ?? ""}
                        onChange={(e) => {
                          const value = e.currentTarget?.value ?? "";
                          setContextValues((prev) => ({
                            ...prev,
                            [field.key]: value,
                          }));
                        }}
                        size="xs"
                      />
                    ))}
                  </Stack>
                </Collapse>
              </Paper>
            )}

            <MessageList
              messages={messages}
              isLoading={isLoading}
              progressMessage={progressMessage}
            />

            <MessageInput onSend={handleSendMessage} disabled={isLoading} />
          </>
        )}
      </Box>
    </Flex>
  );
}
