import fs from "fs/promises";
import path from "path";
import {
  customRelationsAugmentationSchema,
  computedColumnsAugmentationSchema,
  type CustomRelationsAugmentation,
  type ComputedColumnsAugmentation,
  columnConversionsAugmentationSchema,
  type ColumnConversionsAugmentation,
} from "../types/customSchema";
import { logger } from "./logger";

const schemaAugmentationsRoot =
  process.env.SCHEMA_AUGMENTATIONS_DIR ||
  path.resolve(process.cwd(), "schema-augmentations");

const SCHEMA_AUGMENTATIONS_DIR = path.resolve(schemaAugmentationsRoot);
const CUSTOM_RELATIONS_FILE = path.join(
  SCHEMA_AUGMENTATIONS_DIR,
  "custom-relations.json",
);
const COMPUTED_COLUMNS_FILE = path.join(
  SCHEMA_AUGMENTATIONS_DIR,
  "computed-columns.json",
);
const COLUMN_CONVERSIONS_FILE = path.join(
  SCHEMA_AUGMENTATIONS_DIR,
  "column-conversions.json",
);

async function ensureSchemaAugmentationsDir() {
  await fs.mkdir(SCHEMA_AUGMENTATIONS_DIR, { recursive: true });
}

async function writeAugmentationFile(filePath: string, payload: unknown) {
  await ensureSchemaAugmentationsDir();
  const tmpPath = `${filePath}.tmp`;
  await fs.writeFile(tmpPath, JSON.stringify(payload, null, 2), "utf8");
  await fs.rename(tmpPath, filePath);
}

async function readAugmentationFile<T>(
  filePath: string,
  fallback: T,
): Promise<T> {
  try {
    const contents = await fs.readFile(filePath, "utf8");
    return JSON.parse(contents) as T;
  } catch (error: any) {
    if (error.code === "ENOENT") {
      return fallback;
    }
    logger.warn(
      { error, filePath },
      "Schema augmentation store - failed to read augmentation file",
    );
    return fallback;
  }
}

export async function writeCustomRelationsAugmentation(
  augmentation: CustomRelationsAugmentation,
) {
  const payload = customRelationsAugmentationSchema.parse(augmentation);
  await writeAugmentationFile(CUSTOM_RELATIONS_FILE, payload);
}

export async function readCustomRelationsAugmentation(): Promise<CustomRelationsAugmentation> {
  const raw = await readAugmentationFile<unknown>(CUSTOM_RELATIONS_FILE, {
    relations: [],
  });
  const parsed = customRelationsAugmentationSchema.safeParse(raw);
  if (parsed.success) {
    return parsed.data;
  }
  logger.warn(
    { issues: parsed.error.issues },
    "Schema augmentation store - invalid custom relations augmentation, returning empty default",
  );
  return { relations: [] };
}

export async function writeComputedColumnsAugmentation(
  augmentation: ComputedColumnsAugmentation,
) {
  const payload = computedColumnsAugmentationSchema.parse(augmentation);
  await writeAugmentationFile(COMPUTED_COLUMNS_FILE, payload);
}

export async function readComputedColumnsAugmentation(): Promise<ComputedColumnsAugmentation> {
  const raw = await readAugmentationFile<unknown>(COMPUTED_COLUMNS_FILE, {
    computedColumns: [],
  });
  const parsed = computedColumnsAugmentationSchema.safeParse(raw);
  if (parsed.success) {
    return parsed.data;
  }
  logger.warn(
    { issues: parsed.error.issues },
    "Schema augmentation store - invalid computed columns augmentation, returning empty default",
  );
  return { computedColumns: [] };
}

export async function writeColumnConversionsAugmentation(
  augmentation: ColumnConversionsAugmentation,
) {
  const payload = columnConversionsAugmentationSchema.parse(augmentation);
  await writeAugmentationFile(COLUMN_CONVERSIONS_FILE, payload);
}

export async function readColumnConversionsAugmentation(): Promise<ColumnConversionsAugmentation> {
  const raw = await readAugmentationFile<unknown>(COLUMN_CONVERSIONS_FILE, {
    columnConversions: [],
  });
  const parsed = columnConversionsAugmentationSchema.safeParse(raw);
  if (parsed.success) {
    return parsed.data;
  }
  logger.warn(
    { issues: parsed.error.issues },
    "Schema augmentation store - invalid column conversions augmentation, returning empty default",
  );
  return { columnConversions: [] };
}
