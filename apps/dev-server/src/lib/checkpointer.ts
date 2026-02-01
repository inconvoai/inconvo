import { SqliteSaver } from "@langchain/langgraph-checkpoint-sqlite";
import path from "path";

/**
 * SQLite-based checkpointer for LangGraph conversation state
 * Persists state across server restarts
 */

let checkpointerInstance: SqliteSaver | null = null;

export function getCheckpointer(): SqliteSaver {
  if (!checkpointerInstance) {
    // Use same database as Prisma - SqliteSaver creates its own tables
    const dbPath =
      process.env.INCONVO_LOCAL_DB_PATH ??
      path.join(process.cwd(), "prisma", ".inconvo.db");
    checkpointerInstance = SqliteSaver.fromConnString(dbPath);
  }
  return checkpointerInstance;
}
