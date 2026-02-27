import path from "path";
import { PrismaLibSql } from "@prisma/adapter-libsql";
import { PrismaClient } from "../../prisma/generated/client/client";

// Singleton pattern for Prisma client with libSQL adapter (local SQLite)
declare global {
  var prisma: PrismaClient | undefined;
}

function createPrismaClient(): PrismaClient {
  // Use INCONVO_LOCAL_DB_PATH if set, otherwise default to local prisma dir
  const dbPath =
    process.env.INCONVO_LOCAL_DB_PATH ??
    path.join(process.cwd(), "prisma", ".inconvo.db");
  const adapter = new PrismaLibSql({ url: `file:${dbPath}` });
  return new PrismaClient({ adapter });
}

export const prisma = globalThis.prisma ?? createPrismaClient();

if (process.env.NODE_ENV !== "production") {
  globalThis.prisma = prisma;
}
