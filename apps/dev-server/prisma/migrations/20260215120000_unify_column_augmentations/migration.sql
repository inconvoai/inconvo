-- CreateTable
CREATE TABLE "ColumnAugmentation" (
    "id" TEXT NOT NULL PRIMARY KEY,
    "kind" TEXT NOT NULL,
    "selected" BOOLEAN NOT NULL DEFAULT true,
    "columnId" TEXT NOT NULL,
    "tableId" TEXT NOT NULL,
    CONSTRAINT "ColumnAugmentation_columnId_fkey" FOREIGN KEY ("columnId") REFERENCES "Column" ("id") ON DELETE CASCADE ON UPDATE CASCADE,
    CONSTRAINT "ColumnAugmentation_tableId_fkey" FOREIGN KEY ("tableId") REFERENCES "Table" ("id") ON DELETE CASCADE ON UPDATE CASCADE
);

-- CreateTable
CREATE TABLE "ColumnAugmentationConversion" (
    "id" TEXT NOT NULL PRIMARY KEY,
    "ast" TEXT NOT NULL,
    "type" TEXT,
    "augmentationId" TEXT NOT NULL,
    CONSTRAINT "ColumnAugmentationConversion_augmentationId_fkey" FOREIGN KEY ("augmentationId") REFERENCES "ColumnAugmentation" ("id") ON DELETE CASCADE ON UPDATE CASCADE
);

-- CreateTable
CREATE TABLE "ColumnAugmentationStaticEnum" (
    "id" TEXT NOT NULL PRIMARY KEY,
    "entries" TEXT NOT NULL,
    "augmentationId" TEXT NOT NULL,
    CONSTRAINT "ColumnAugmentationStaticEnum_augmentationId_fkey" FOREIGN KEY ("augmentationId") REFERENCES "ColumnAugmentation" ("id") ON DELETE CASCADE ON UPDATE CASCADE
);

-- CreateTable
CREATE TABLE "ColumnAugmentationDynamicEnum" (
    "id" TEXT NOT NULL PRIMARY KEY,
    "augmentationId" TEXT NOT NULL,
    CONSTRAINT "ColumnAugmentationDynamicEnum_augmentationId_fkey" FOREIGN KEY ("augmentationId") REFERENCES "ColumnAugmentation" ("id") ON DELETE CASCADE ON UPDATE CASCADE
);

-- CreateIndex
CREATE UNIQUE INDEX "ColumnAugmentation_columnId_key" ON "ColumnAugmentation"("columnId");

-- CreateIndex
CREATE INDEX "ColumnAugmentation_tableId_idx" ON "ColumnAugmentation"("tableId");

-- CreateIndex
CREATE UNIQUE INDEX "ColumnAugmentationConversion_augmentationId_key" ON "ColumnAugmentationConversion"("augmentationId");

-- CreateIndex
CREATE UNIQUE INDEX "ColumnAugmentationStaticEnum_augmentationId_key" ON "ColumnAugmentationStaticEnum"("augmentationId");

-- CreateIndex
CREATE UNIQUE INDEX "ColumnAugmentationDynamicEnum_augmentationId_key" ON "ColumnAugmentationDynamicEnum"("augmentationId");

-- Backfill conversions first (conversions win if both conversion + enum exist)
INSERT INTO "ColumnAugmentation" (
    "id",
    "kind",
    "selected",
    "columnId",
    "tableId"
)
SELECT
    'aug_conv_' || cc."id",
    'CONVERSION',
    cc."selected",
    cc."columnId",
    cc."tableId"
FROM "ColumnConversion" cc;

INSERT INTO "ColumnAugmentationConversion" (
    "id",
    "ast",
    "type",
    "augmentationId"
)
SELECT
    'aug_conv_cfg_' || cc."id",
    cc."ast",
    cc."type",
    ca."id"
FROM "ColumnConversion" cc
INNER JOIN "ColumnAugmentation" ca
    ON ca."columnId" = cc."columnId"
    AND ca."kind" = 'CONVERSION';

-- Backfill enums for columns without a conversion
INSERT INTO "ColumnAugmentation" (
    "id",
    "kind",
    "selected",
    "columnId",
    "tableId"
)
SELECT
    'aug_enum_' || ve."id",
    'STATIC_ENUM',
    ve."selected",
    ve."columnId",
    ve."tableId"
FROM "ColumnValueEnum" ve
LEFT JOIN "ColumnConversion" cc
    ON cc."columnId" = ve."columnId"
WHERE cc."id" IS NULL;

INSERT INTO "ColumnAugmentationStaticEnum" (
    "id",
    "entries",
    "augmentationId"
)
SELECT
    'aug_enum_cfg_' || ve."id",
    ve."entries",
    ca."id"
FROM "ColumnValueEnum" ve
INNER JOIN "ColumnAugmentation" ca
    ON ca."columnId" = ve."columnId"
    AND ca."kind" = 'STATIC_ENUM';

-- Drop old models
DROP TABLE "ColumnConversion";
DROP TABLE "ColumnValueEnum";
