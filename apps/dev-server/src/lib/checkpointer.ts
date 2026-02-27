import { BunSqliteSaver } from "./bun-sqlite-saver";
import path from "path";

/**
 * SQLite-based checkpointer for LangGraph conversation state
 * Uses bun:sqlite for Bun runtime compatibility
 * Persists state across server restarts
 */

let checkpointerInstance: BunSqliteSaver | null = null;

export function getCheckpointer(): BunSqliteSaver {
  if (!checkpointerInstance) {
    // Use same database as Prisma - BunSqliteSaver creates its own tables
    const dbPath =
      process.env.INCONVO_LOCAL_DB_PATH ??
      path.join(process.cwd(), "prisma", ".inconvo.db");
    checkpointerInstance = new BunSqliteSaver(dbPath);
  }
  return checkpointerInstance;
}
