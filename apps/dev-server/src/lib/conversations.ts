import type { Conversation } from "@repo/types";
import { v4 as uuidv4 } from "uuid";

/**
 * In-memory conversation store
 * Conversations are lost on server restart
 */
const conversations = new Map<string, Conversation>();

export interface ConversationWithMeta extends Conversation {
  createdAt: Date;
  updatedAt: Date;
}

const conversationMeta = new Map<
  string,
  { createdAt: Date; updatedAt: Date }
>();

export function createConversation(
  requestContext?: Record<string, string | number> | null,
): Conversation {
  const id = `convo_${uuidv4()}`;
  const conversation: Conversation = {
    id,
    title: null,
    requestContext: requestContext ?? null,
  };

  conversations.set(id, conversation);
  conversationMeta.set(id, {
    createdAt: new Date(),
    updatedAt: new Date(),
  });

  return conversation;
}

export function getConversation(id: string): Conversation | undefined {
  return conversations.get(id);
}

export function updateConversation(
  id: string,
  updates: Partial<Omit<Conversation, "id">>,
): Conversation | undefined {
  const existing = conversations.get(id);
  if (!existing) return undefined;

  const updated: Conversation = {
    ...existing,
    ...updates,
  };
  conversations.set(id, updated);

  const meta = conversationMeta.get(id);
  if (meta) {
    meta.updatedAt = new Date();
  }

  return updated;
}

export function listConversations(): ConversationWithMeta[] {
  const result: ConversationWithMeta[] = [];

  for (const [id, conversation] of conversations) {
    const meta = conversationMeta.get(id);
    if (meta) {
      result.push({
        ...conversation,
        createdAt: meta.createdAt,
        updatedAt: meta.updatedAt,
      });
    }
  }

  // Sort by updatedAt descending (most recent first)
  return result.sort((a, b) => b.updatedAt.getTime() - a.updatedAt.getTime());
}

export function deleteConversation(id: string): boolean {
  conversationMeta.delete(id);
  return conversations.delete(id);
}
