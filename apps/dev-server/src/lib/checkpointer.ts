import { MemorySaver } from "@langchain/langgraph";

/**
 * In-memory checkpointer for LangGraph conversation state
 * State is lost on server restart
 */

// Singleton instance
let checkpointerInstance: MemorySaver | null = null;

export function getCheckpointer(): MemorySaver {
  checkpointerInstance ??= new MemorySaver();
  return checkpointerInstance;
}
