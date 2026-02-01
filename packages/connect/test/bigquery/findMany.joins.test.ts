// @ts-nocheck
import { sql, type Kysely } from "kysely";
import { loadTestEnv, getTestContext } from "../loadTestEnv";
import {
  setupMultiHopFixture,
  teardownMultiHopFixture,
  type MultiHopFixture,
} from "../utils/multiHopFixture";

describe("BigQuery findMany Join Edge Cases", () => {
  let db: Kysely<any>;
  let QuerySchema: (typeof import("~/types/querySchema"))["QuerySchema"];
  let findMany: (typeof import("~/operations/findMany"))["findMany"];
  let clearSchemaCache: (typeof import("~/util/schemaCache"))["clearSchemaCache"];
  let clearAugmentedSchemaCache:
    | (typeof import("~/util/augmentedSchemaCache"))["clearAugmentedSchemaCache"]
    | undefined;
  let ctx: Awaited<ReturnType<typeof getTestContext>>;
  let multiHopFixture: MultiHopFixture | null = null;

  beforeAll(async () => {
    jest.setTimeout(120000);
    loadTestEnv("bigquery");

    jest.resetModules();
    delete (globalThis as any).__INCONVO_KYSELY_DB__;

    ({ clearSchemaCache } = await import("~/util/schemaCache"));
    ({ clearAugmentedSchemaCache } = await import("~/util/augmentedSchemaCache"));

    QuerySchema = (await import("~/types/querySchema")).QuerySchema;
    findMany = (await import("~/operations/findMany")).findMany;
    const { getDb } = await import("~/dbConnection");
    db = await getDb();
    multiHopFixture = await setupMultiHopFixture(db);

    await clearSchemaCache();
    clearAugmentedSchemaCache?.();
    ctx = await getTestContext();
  });

  afterAll(async () => {
    if (db && multiHopFixture) {
      await teardownMultiHopFixture(db, multiHopFixture);
    }
    await db?.destroy?.();
    await clearSchemaCache?.();
    clearAugmentedSchemaCache?.();
  });

  test("deduplicates shared first hop in multi-hop joins", async () => {
    if (!multiHopFixture) {
      throw new Error("Missing multi-hop fixture setup.");
    }

    const aliasPrimary = "primaryMid";
    const aliasSecondary = "secondaryMid";
    const aliasEnd = `${aliasSecondary}.end`;

    const baseIdKey = `${multiHopFixture.baseTable}_${multiHopFixture.baseColumns.id}`;
    const basePrimaryKey = `${multiHopFixture.baseTable}_${multiHopFixture.baseColumns.primaryMidId}`;
    const baseSecondaryKey = `${multiHopFixture.baseTable}_${multiHopFixture.baseColumns.secondaryMidId}`;
    const primaryMidKey = `${aliasPrimary}_${multiHopFixture.midColumns.id}`;
    const secondaryMidKey = `${aliasSecondary}_${multiHopFixture.midColumns.id}`;
    const endLabelKey = `${aliasEnd}_${multiHopFixture.endColumns.label}`;

    const iql = {
      operation: "findMany" as const,
      table: multiHopFixture.baseTable,
      tableConditions: null,
      whereAndArray: [],
      operationParameters: {
        select: {
          [multiHopFixture.baseTable]: [
            multiHopFixture.baseColumns.id,
            multiHopFixture.baseColumns.primaryMidId,
            multiHopFixture.baseColumns.secondaryMidId,
          ],
          [aliasPrimary]: [multiHopFixture.midColumns.id],
          [aliasSecondary]: [multiHopFixture.midColumns.id],
          [aliasEnd]: [multiHopFixture.endColumns.label],
        },
        joins: [
          {
            table: multiHopFixture.midTable,
            name: aliasPrimary,
            path: [
              {
                source: [
                  `${multiHopFixture.baseTable}.${multiHopFixture.baseColumns.primaryMidId}`,
                ],
                target: [
                  `${multiHopFixture.midTable}.${multiHopFixture.midColumns.id}`,
                ],
              },
            ],
          },
          {
            table: multiHopFixture.midTable,
            name: aliasSecondary,
            path: [
              {
                source: [
                  `${multiHopFixture.baseTable}.${multiHopFixture.baseColumns.secondaryMidId}`,
                ],
                target: [
                  `${multiHopFixture.midTable}.${multiHopFixture.midColumns.id}`,
                ],
              },
            ],
          },
          {
            table: multiHopFixture.endTable,
            name: aliasEnd,
            path: [
              {
                source: [
                  `${multiHopFixture.baseTable}.${multiHopFixture.baseColumns.secondaryMidId}`,
                ],
                target: [
                  `${multiHopFixture.midTable}.${multiHopFixture.midColumns.id}`,
                ],
              },
              {
                source: [
                  `${multiHopFixture.midTable}.${multiHopFixture.midColumns.endId}`,
                ],
                target: [
                  `${multiHopFixture.endTable}.${multiHopFixture.endColumns.id}`,
                ],
              },
            ],
          },
        ],
        orderBy: {
          column: multiHopFixture.baseColumns.id,
          direction: "asc" as const,
        },
        limit: 10,
      },
    };

    const parsed = QuerySchema.parse(iql);
    const response = await findMany(db, parsed, ctx);

    const expectedRows = await db
      .selectFrom(`${multiHopFixture.baseTable} as base`)
      .leftJoin(
        `${multiHopFixture.midTable} as primary_mid`,
        `primary_mid.${multiHopFixture.midColumns.id}`,
        `base.${multiHopFixture.baseColumns.primaryMidId}`,
      )
      .leftJoin(
        `${multiHopFixture.midTable} as secondary_mid`,
        `secondary_mid.${multiHopFixture.midColumns.id}`,
        `base.${multiHopFixture.baseColumns.secondaryMidId}`,
      )
      .leftJoin(
        `${multiHopFixture.endTable} as end_tbl`,
        `end_tbl.${multiHopFixture.endColumns.id}`,
        `secondary_mid.${multiHopFixture.midColumns.endId}`,
      )
      .select([
        sql`${sql.ref("base")}.${sql.ref(multiHopFixture.baseColumns.id)}`.as(
          baseIdKey,
        ),
        sql`${sql.ref("base")}.${sql.ref(multiHopFixture.baseColumns.primaryMidId)}`.as(
          basePrimaryKey,
        ),
        sql`${sql.ref("base")}.${sql.ref(multiHopFixture.baseColumns.secondaryMidId)}`.as(
          baseSecondaryKey,
        ),
        sql`${sql.ref("primary_mid")}.${sql.ref(multiHopFixture.midColumns.id)}`.as(
          primaryMidKey,
        ),
        sql`${sql.ref("secondary_mid")}.${sql.ref(multiHopFixture.midColumns.id)}`.as(
          secondaryMidKey,
        ),
        sql`${sql.ref("end_tbl")}.${sql.ref(multiHopFixture.endColumns.label)}`.as(
          endLabelKey,
        ),
      ])
      .orderBy(`base.${multiHopFixture.baseColumns.id}`, "asc")
      .limit(10)
      .execute();

    expect(response.data).toEqual(expectedRows);
  }, 150000);
});
