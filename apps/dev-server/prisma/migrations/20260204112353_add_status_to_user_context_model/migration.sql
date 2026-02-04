-- CreateTable
CREATE TABLE "UserContextConfig" (
    "id" TEXT NOT NULL PRIMARY KEY,
    "status" TEXT NOT NULL DEFAULT 'UNSET'
);

-- Backfill: enable user context when fields already exist
INSERT INTO "UserContextConfig" ("id", "status")
SELECT
  'default',
  CASE
    WHEN EXISTS (SELECT 1 FROM "UserContextField") THEN 'ENABLED'
    ELSE 'UNSET'
  END
WHERE NOT EXISTS (SELECT 1 FROM "UserContextConfig");
