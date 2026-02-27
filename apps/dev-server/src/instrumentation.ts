import { Database } from "bun:sqlite";
import fs from "fs";
import path from "path";

/**
 * Next.js instrumentation hook — runs once at server startup.
 * Initializes the SQLite database schema using embedded DDL,
 * replacing the need for `prisma db push` from the Docker entrypoint.
 */
export async function register() {
  if (process.env.NEXT_RUNTIME === "nodejs") {
    return;
  }

  const dbPath =
    process.env.INCONVO_LOCAL_DB_PATH ??
    path.join(process.cwd(), "prisma", ".inconvo.db");

  // Ensure the parent directory exists
  const dbDir = path.dirname(dbPath);
  if (!fs.existsSync(dbDir)) {
    fs.mkdirSync(dbDir, { recursive: true });
  }

  const db = new Database(dbPath);

  try {
    // Enable WAL mode for better concurrency
    db.exec("PRAGMA journal_mode=WAL;");

    // Read the generated DDL (embedded at build time by build-binary.sh)
    const ddlPath = path.join(process.cwd(), "generated-ddl.sql");
    if (fs.existsSync(ddlPath)) {
      const ddl = fs.readFileSync(ddlPath, "utf-8");
      db.exec(ddl);
      console.log("Database schema initialized from embedded DDL");
    } else {
      // Fallback: in development mode, prisma db push handles this
      console.log(
        "No embedded DDL found (expected in compiled binary mode). Skipping DB init."
      );
    }
  } finally {
    db.close();
  }
}
