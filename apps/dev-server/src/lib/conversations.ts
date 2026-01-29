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

export interface ConversationWithMetaAndMessages extends ConversationWithMeta {
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

/**
 * List conversations with filtering and pagination
 *
 * When userContext filtering is applied, iteratively fetches pages until
 * we have limit + 1 filtered results to ensure correct pagination.
 */
export async function listConversationsFiltered(params: {
  cursor?: Date;
  limit: number;
  userIdentifier?: string;
  userContext?: Record<string, string | number>;
}): Promise<{
  conversations: ConversationWithMetaAndMessages[];
  nextCursor: string | null;
}> {
  const { cursor, limit, userIdentifier, userContext } = params;

  // Build WHERE clause for DB-level filtering
  const where: {
    userIdentifier?: string;
    updatedAt?: { lt: Date };
  } = {};

  if (userIdentifier) {
    where.userIdentifier = userIdentifier;
  }

  if (cursor) {
    where.updatedAt = { lt: cursor };
  }

  // If no userContext filter, simple case: fetch limit + 1 and we're done
  if (!userContext) {
    const conversations = await prisma.conversation.findMany({
      where,
      orderBy: { updatedAt: "desc" },
      take: limit + 1,
    });

    const hasMore = conversations.length > limit;
    const results = hasMore ? conversations.slice(0, limit) : conversations;

    const lastItem = results[results.length - 1];
    const nextCursor =
      hasMore && lastItem
        ? Buffer.from(lastItem.updatedAt.toISOString()).toString("base64")
        : null;

    return {
      conversations: results.map((c) => ({
        ...toConversation(c),
        createdAt: c.createdAt,
        updatedAt: c.updatedAt,
        messages: parseMessages(c.messages),
      })),
      nextCursor,
    };
  }

  // If userContext filter is provided, iteratively fetch until we have limit + 1 filtered results
  type ConversationRow = {
    id: string;
    userIdentifier: string;
    title: string | null;
    userContext: string | null;
    messages: string;
    createdAt: Date;
    updatedAt: Date;
  };

  const filtered: ConversationRow[] = [];
  let fetchCursor = cursor;
  const batchSize = 50;

  // Keep fetching batches until we have enough filtered results or run out of data
  while (true) {
    const batchWhere = { ...where };
    if (fetchCursor) {
      batchWhere.updatedAt = { lt: fetchCursor };
    }

    const batch = await prisma.conversation.findMany({
      where: batchWhere,
      orderBy: { updatedAt: "desc" },
      take: batchSize,
    });

    // No more results in DB - stop
    if (batch.length === 0) break;

    // Filter batch by userContext
    for (const c of batch) {
      const ctx = parseUserContext(c.userContext);

      // Check if all userContext filters match
      if (ctx && Object.entries(userContext).every(([key, value]) => ctx[key] === value)) {
        filtered.push(c);

        // Early exit: we have enough filtered results (limit + 1)
        if (filtered.length > limit) break;
      }
    }

    // If we have enough filtered results, stop fetching
    if (filtered.length > limit) break;

    // Update cursor for next batch
    const lastInBatch = batch[batch.length - 1];
    if (lastInBatch) {
      fetchCursor = lastInBatch.updatedAt;
    }

    // If we got fewer than batchSize, no more results exist in DB
    if (batch.length < batchSize) break;
  }

  // Determine if there are more results
  const hasMore = filtered.length > limit;
  const results = hasMore ? filtered.slice(0, limit) : filtered;

  // Generate next cursor from last returned item
  const lastItem = results[results.length - 1];
  const nextCursor =
    hasMore && lastItem
      ? Buffer.from(lastItem.updatedAt.toISOString()).toString("base64")
      : null;

  return {
    conversations: results.map((c) => ({
      ...toConversation(c),
      createdAt: c.createdAt,
      updatedAt: c.updatedAt,
      messages: parseMessages(c.messages),
    })),
    nextCursor,
  };
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
