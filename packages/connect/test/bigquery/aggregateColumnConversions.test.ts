// @ts-nocheck
import fs from "fs/promises";
import os from "os";
import path from "path";
import { loadTestEnv, getTestContext } from "../loadTestEnv";
import {
  writeAugmentationFile,
} from "../utils/augmentations";

describe("BigQuery aggregate with column conversions", () => {
  let aggregate: (typeof import("~/operations/aggregate"))["aggregate"];
  let QuerySchema: (typeof import("~/types/querySchema"))["QuerySchema"];
  let clearSchemaCache: (typeof import("~/util/schemaCache"))["clearSchemaCache"];
  let clearAugmentedSchemaCache: (typeof import("~/util/augmentedSchemaCache"))["clearAugmentedSchemaCache"];
  let getAugmentedSchema: (typeof import("~/util/augmentedSchemaCache"))["getAugmentedSchema"];
  let getSchemaColumnConversions: (typeof import("~/util/columnConversions"))["getSchemaColumnConversions"];
  let db: import("kysely").Kysely<any>;
  let originalAugmentationsDir: string | undefined;
  let tempAugmentDir: string | null = null;
  let augmentationsFile: string;
  let ctx: Awaited<ReturnType<typeof getTestContext>>;

  beforeAll(async () => {
    jest.setTimeout(120000);
    loadTestEnv("bigquery");

    // Ensure fresh module state + connection
    jest.resetModules();
    delete (globalThis as any).__INCONVO_KYSELY_DB__;

    originalAugmentationsDir = process.env.SCHEMA_AUGMENTATIONS_DIR;
    tempAugmentDir = await fs.mkdtemp(
      path.join(os.tmpdir(), "inconvo-augmentations-"),
    );
    process.env.SCHEMA_AUGMENTATIONS_DIR = tempAugmentDir;
    augmentationsFile = path.resolve(tempAugmentDir, "augmentations.json");

    await writeAugmentationFile(augmentationsFile, {
      relations: [],
      computedColumns: [],
      columnConversions: [
        {
          table: "events",
          column: "properties#total_price",
          // SAFE_CAST(NUMERIC) with null->0 fallback to make SUM deterministic
          ast: {
            type: "coalesce",
            expression: {
              type: "cast",
              as: "number",
              expression: {
                type: "column",
                name: "properties#total_price",
              },
            },
            fallback: {
              type: "value",
              value: 0,
            },
          },
          type: "number",
        },
      ],
    });

    ({ clearSchemaCache } = await import("~/util/schemaCache"));
    ({ clearAugmentedSchemaCache } =
      await import("~/util/augmentedSchemaCache"));
    await clearSchemaCache();
    await clearAugmentedSchemaCache();

    QuerySchema = (await import("~/types/querySchema")).QuerySchema;
    aggregate = (await import("~/operations/aggregate")).aggregate;
    getAugmentedSchema = (await import("~/util/augmentedSchemaCache"))
      .getAugmentedSchema;
    getSchemaColumnConversions = (await import("~/util/columnConversions"))
      .getSchemaColumnConversions;
    const { getDb } = await import("~/dbConnection");
    db = await getDb();
    ctx = await getTestContext();
  });

  afterAll(async () => {
    await db?.destroy?.();
    await clearSchemaCache();
    await clearAugmentedSchemaCache();
    if (originalAugmentationsDir !== undefined) {
      process.env.SCHEMA_AUGMENTATIONS_DIR = originalAugmentationsDir;
    } else {
      delete process.env.SCHEMA_AUGMENTATIONS_DIR;
    }
    if (tempAugmentDir) {
      await fs.rm(tempAugmentDir, { recursive: true, force: true });
    }
  });

  it("sums a STRUCT field using the configured column conversion", async () => {
    await clearSchemaCache();
    await clearAugmentedSchemaCache();

    const schema = await getAugmentedSchema();
    const conversions = getSchemaColumnConversions(schema);
    const hasConversion = conversions?.some(
      (c) => c.table.name === "events" && c.column === "properties#total_price",
    );
    expect(hasConversion).toBe(true);

    const iql = {
      operation: "aggregate" as const,
      table: "events",
      tableConditions: null,
      whereAndArray: [],
      operationParameters: {
        count: null,
        countDistinct: null,
        sum: ["events.properties#total_price"],
        min: null,
        max: null,
        avg: null,
        median: null,
        joins: null,
      },
    };

    const parsed = QuerySchema.parse(iql);
    const response = await aggregate(db, parsed, ctx);

    expect(response.data).toBeDefined();
    const sumValue = response.data?._sum?.["events.properties#total_price"];
    expect(sumValue).not.toBeUndefined();
    const numeric = Number(sumValue);
    expect(Number.isNaN(numeric)).toBe(false);
  });
});
