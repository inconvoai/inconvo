-- AlterTable: add source column to Table (default PHYSICAL for all existing rows)
ALTER TABLE "Table" ADD COLUMN "source" TEXT NOT NULL DEFAULT 'PHYSICAL';

-- CreateTable
CREATE TABLE "VirtualTableConfig" (
    "id" TEXT NOT NULL PRIMARY KEY,
    "tableId" TEXT NOT NULL,
    "dialect" TEXT NOT NULL,
    "sql" TEXT NOT NULL,
    "createdAt" DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP,
    "updatedAt" DATETIME NOT NULL,
    CONSTRAINT "VirtualTableConfig_tableId_fkey" FOREIGN KEY ("tableId") REFERENCES "Table" ("id") ON DELETE CASCADE ON UPDATE CASCADE
);

-- CreateIndex
CREATE UNIQUE INDEX "VirtualTableConfig_tableId_key" ON "VirtualTableConfig"("tableId");
