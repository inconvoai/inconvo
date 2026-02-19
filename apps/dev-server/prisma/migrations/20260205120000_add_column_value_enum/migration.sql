-- CreateTable
CREATE TABLE "ColumnValueEnum" (
    "id" TEXT NOT NULL PRIMARY KEY,
    "entries" TEXT NOT NULL,
    "selected" BOOLEAN NOT NULL DEFAULT true,
    "columnId" TEXT NOT NULL,
    "tableId" TEXT NOT NULL,
    CONSTRAINT "ColumnValueEnum_columnId_fkey" FOREIGN KEY ("columnId") REFERENCES "Column" ("id") ON DELETE CASCADE ON UPDATE CASCADE,
    CONSTRAINT "ColumnValueEnum_tableId_fkey" FOREIGN KEY ("tableId") REFERENCES "Table" ("id") ON DELETE CASCADE ON UPDATE CASCADE
);

-- CreateIndex
CREATE UNIQUE INDEX "ColumnValueEnum_columnId_key" ON "ColumnValueEnum"("columnId");

-- CreateIndex
CREATE INDEX "ColumnValueEnum_tableId_idx" ON "ColumnValueEnum"("tableId");
