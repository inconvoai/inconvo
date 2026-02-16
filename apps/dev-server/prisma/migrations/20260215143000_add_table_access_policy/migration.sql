-- CreateTable
CREATE TABLE "TableAccessPolicy" (
    "id" TEXT NOT NULL PRIMARY KEY,
    "tableId" TEXT NOT NULL,
    "userContextFieldId" TEXT NOT NULL,
    CONSTRAINT "TableAccessPolicy_tableId_fkey" FOREIGN KEY ("tableId") REFERENCES "Table" ("id") ON DELETE CASCADE ON UPDATE CASCADE,
    CONSTRAINT "TableAccessPolicy_userContextFieldId_fkey" FOREIGN KEY ("userContextFieldId") REFERENCES "UserContextField" ("id") ON DELETE CASCADE ON UPDATE CASCADE
);

-- CreateIndex
CREATE UNIQUE INDEX "TableAccessPolicy_tableId_key" ON "TableAccessPolicy"("tableId");

-- CreateIndex
CREATE INDEX "TableAccessPolicy_userContextFieldId_idx" ON "TableAccessPolicy"("userContextFieldId");
