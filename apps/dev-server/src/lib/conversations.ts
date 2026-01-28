import type { Conversation, InconvoResponse } from "@repo/types";
import { v4 as uuidv4 } from "uuid";
import { prisma } from "./prisma";

export interface ChatMessage {
  id: string;
  type: "user" | InconvoResponse["type"];
  content: string | InconvoResponse;
}

export interface ConversationWithMeta extends Conversation {
  createdAt: Date;
  updatedAt: Date;
}

export interface ConversationWithMessages extends Conversation {
  messages: ChatMessage[];
}

/**
 * Parse userContext from JSON string (SQLite storage) to object
 */
function parseUserContext(
  userContext: string | null,
): Record<string, string | number> | null {
  if (!userContext) return null;
  try {
    return JSON.parse(userContext) as Record<string, string | number>;
  } catch {
    return null;
  }
}

/**
 * Parse messages from JSON string (SQLite storage) to array
 */
function parseMessages(messages: string): ChatMessage[] {
  try {
    return JSON.parse(messages) as ChatMessage[];
  } catch {
    return [];
  }
}

/**
 * Convert Prisma model to Conversation type
 */
function toConversation(row: {
  id: string;
  userIdentifier: string;
  title: string | null;
  userContext: string | null;
}): Conversation {
  return {
    id: row.id,
    userIdentifier: row.userIdentifier,
    title: row.title,
    userContext: parseUserContext(row.userContext),
  };
}

/**
 * Convert Prisma model to ConversationWithMessages type
 */
function toConversationWithMessages(row: {
  id: string;
  userIdentifier: string;
  title: string | null;
  userContext: string | null;
  messages: string;
}): ConversationWithMessages {
  return {
    id: row.id,
    userIdentifier: row.userIdentifier,
    title: row.title,
    userContext: parseUserContext(row.userContext),
    messages: parseMessages(row.messages),
  };
}

export async function createConversation(
  userIdentifier: string,
  userContext?: Record<string, string | number> | null,
): Promise<Conversation> {
  const id = `convo_${uuidv4()}`;

  const conversation = await prisma.conversation.create({
    data: {
      id,
      userIdentifier,
      userContext: userContext ? JSON.stringify(userContext) : null,
    },
  });

  return toConversation(conversation);
}

export async function getConversation(
  id: string,
): Promise<Conversation | undefined> {
  const conversation = await prisma.conversation.findUnique({
    where: { id },
  });

  if (!conversation) return undefined;
  return toConversation(conversation);
}

export async function getConversationWithMessages(
  id: string,
): Promise<ConversationWithMessages | undefined> {
  const conversation = await prisma.conversation.findUnique({
    where: { id },
  });

  if (!conversation) return undefined;
  return toConversationWithMessages(conversation);
}

export async function updateConversation(
  id: string,
  updates: Partial<Omit<Conversation, "id">>,
): Promise<Conversation | undefined> {
  try {
    const data: {
      userIdentifier?: string;
      title?: string | null;
      userContext?: string | null;
    } = {};

    if (updates.userIdentifier !== undefined) {
      data.userIdentifier = updates.userIdentifier;
    }
    if (updates.title !== undefined) {
      data.title = updates.title;
    }
    if (updates.userContext !== undefined) {
      data.userContext = updates.userContext
        ? JSON.stringify(updates.userContext)
        : null;
    }

    const conversation = await prisma.conversation.update({
      where: { id },
      data,
    });

    return toConversation(conversation);
  } catch {
    // Record not found
    return undefined;
  }
}

export async function appendMessages(
  id: string,
  newMessages: ChatMessage[],
): Promise<void> {
  const conversation = await prisma.conversation.findUnique({
    where: { id },
    select: { messages: true },
  });

  if (!conversation) return;

  const existingMessages = parseMessages(conversation.messages);
  const updatedMessages = [...existingMessages, ...newMessages];

  await prisma.conversation.update({
    where: { id },
    data: { messages: JSON.stringify(updatedMessages) },
  });
}

export async function listConversations(): Promise<ConversationWithMeta[]> {
  const conversations = await prisma.conversation.findMany({
    orderBy: { updatedAt: "desc" },
  });

  return conversations.map((c) => ({
    ...toConversation(c),
    createdAt: c.createdAt,
    updatedAt: c.updatedAt,
  }));
}

export async function deleteConversation(id: string): Promise<boolean> {
  try {
    await prisma.conversation.delete({
      where: { id },
    });
    return true;
  } catch {
    return false;
  }
}
