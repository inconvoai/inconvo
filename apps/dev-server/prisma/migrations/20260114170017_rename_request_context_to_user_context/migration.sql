-- CreateTable
CREATE TABLE "Table" (
    "id" TEXT NOT NULL PRIMARY KEY,
    "name" TEXT NOT NULL,
    "access" TEXT NOT NULL DEFAULT 'OFF',
    "context" TEXT,
    "createdAt" DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP,
    "updatedAt" DATETIME NOT NULL
);

-- CreateTable
CREATE TABLE "Column" (
    "id" TEXT NOT NULL PRIMARY KEY,
    "name" TEXT NOT NULL,
    "rename" TEXT,
    "notes" TEXT,
    "type" TEXT NOT NULL,
    "selected" BOOLEAN NOT NULL DEFAULT true,
    "unit" TEXT,
    "tableId" TEXT NOT NULL,
    "createdAt" DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP,
    "updatedAt" DATETIME NOT NULL,
    CONSTRAINT "Column_tableId_fkey" FOREIGN KEY ("tableId") REFERENCES "Table" ("id") ON DELETE CASCADE ON UPDATE CASCADE
);

-- CreateTable
CREATE TABLE "ComputedColumn" (
    "id" TEXT NOT NULL PRIMARY KEY,
    "name" TEXT NOT NULL,
    "type" TEXT NOT NULL,
    "ast" TEXT NOT NULL,
    "selected" BOOLEAN NOT NULL DEFAULT true,
    "unit" TEXT,
    "notes" TEXT,
    "tableId" TEXT NOT NULL,
    "createdAt" DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP,
    "updatedAt" DATETIME NOT NULL,
    CONSTRAINT "ComputedColumn_tableId_fkey" FOREIGN KEY ("tableId") REFERENCES "Table" ("id") ON DELETE CASCADE ON UPDATE CASCADE
);

-- CreateTable
CREATE TABLE "Relation" (
    "id" TEXT NOT NULL PRIMARY KEY,
    "name" TEXT NOT NULL,
    "isList" BOOLEAN NOT NULL,
    "selected" BOOLEAN NOT NULL DEFAULT true,
    "source" TEXT NOT NULL DEFAULT 'FK',
    "status" TEXT NOT NULL DEFAULT 'VALID',
    "errorTag" TEXT,
    "sourceTableId" TEXT NOT NULL,
    "targetTableId" TEXT NOT NULL,
    "createdAt" DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP,
    "updatedAt" DATETIME NOT NULL,
    CONSTRAINT "Relation_sourceTableId_fkey" FOREIGN KEY ("sourceTableId") REFERENCES "Table" ("id") ON DELETE CASCADE ON UPDATE CASCADE,
    CONSTRAINT "Relation_targetTableId_fkey" FOREIGN KEY ("targetTableId") REFERENCES "Table" ("id") ON DELETE CASCADE ON UPDATE CASCADE
);

-- CreateTable
CREATE TABLE "RelationColumnMapping" (
    "id" TEXT NOT NULL PRIMARY KEY,
    "relationId" TEXT NOT NULL,
    "sourceColumnId" TEXT,
    "targetColumnId" TEXT,
    "sourceColumnName" TEXT NOT NULL,
    "targetColumnName" TEXT NOT NULL,
    "position" INTEGER NOT NULL,
    CONSTRAINT "RelationColumnMapping_relationId_fkey" FOREIGN KEY ("relationId") REFERENCES "Relation" ("id") ON DELETE CASCADE ON UPDATE CASCADE,
    CONSTRAINT "RelationColumnMapping_sourceColumnId_fkey" FOREIGN KEY ("sourceColumnId") REFERENCES "Column" ("id") ON DELETE SET NULL ON UPDATE CASCADE,
    CONSTRAINT "RelationColumnMapping_targetColumnId_fkey" FOREIGN KEY ("targetColumnId") REFERENCES "Column" ("id") ON DELETE SET NULL ON UPDATE CASCADE
);

-- CreateTable
CREATE TABLE "ColumnConversion" (
    "id" TEXT NOT NULL PRIMARY KEY,
    "ast" TEXT NOT NULL,
    "type" TEXT,
    "selected" BOOLEAN NOT NULL DEFAULT true,
    "columnId" TEXT NOT NULL,
    "tableId" TEXT NOT NULL,
    CONSTRAINT "ColumnConversion_columnId_fkey" FOREIGN KEY ("columnId") REFERENCES "Column" ("id") ON DELETE CASCADE ON UPDATE CASCADE,
    CONSTRAINT "ColumnConversion_tableId_fkey" FOREIGN KEY ("tableId") REFERENCES "Table" ("id") ON DELETE CASCADE ON UPDATE CASCADE
);

-- CreateTable
CREATE TABLE "TableCondition" (
    "id" TEXT NOT NULL PRIMARY KEY,
    "columnId" TEXT NOT NULL,
    "tableId" TEXT NOT NULL,
    "userContextFieldId" TEXT NOT NULL,
    CONSTRAINT "TableCondition_columnId_fkey" FOREIGN KEY ("columnId") REFERENCES "Column" ("id") ON DELETE CASCADE ON UPDATE CASCADE,
    CONSTRAINT "TableCondition_tableId_fkey" FOREIGN KEY ("tableId") REFERENCES "Table" ("id") ON DELETE CASCADE ON UPDATE CASCADE,
    CONSTRAINT "TableCondition_userContextFieldId_fkey" FOREIGN KEY ("userContextFieldId") REFERENCES "UserContextField" ("id") ON DELETE CASCADE ON UPDATE CASCADE
);

-- CreateTable
CREATE TABLE "UserContextField" (
    "id" TEXT NOT NULL PRIMARY KEY,
    "key" TEXT NOT NULL,
    "type" TEXT NOT NULL
);

-- CreateIndex
CREATE UNIQUE INDEX "Table_name_key" ON "Table"("name");

-- CreateIndex
CREATE UNIQUE INDEX "Column_tableId_name_key" ON "Column"("tableId", "name");

-- CreateIndex
CREATE UNIQUE INDEX "ComputedColumn_tableId_name_key" ON "ComputedColumn"("tableId", "name");

-- CreateIndex
CREATE UNIQUE INDEX "Relation_sourceTableId_name_key" ON "Relation"("sourceTableId", "name");

-- CreateIndex
CREATE UNIQUE INDEX "RelationColumnMapping_relationId_position_key" ON "RelationColumnMapping"("relationId", "position");

-- CreateIndex
CREATE UNIQUE INDEX "ColumnConversion_columnId_key" ON "ColumnConversion"("columnId");

-- CreateIndex
CREATE INDEX "ColumnConversion_tableId_idx" ON "ColumnConversion"("tableId");

-- CreateIndex
CREATE UNIQUE INDEX "TableCondition_tableId_key" ON "TableCondition"("tableId");

-- CreateIndex
CREATE UNIQUE INDEX "UserContextField_key_key" ON "UserContextField"("key");
