import { computeAugmentationsHash } from "../src/util/schemaAugmentationStore";
import { getColumnFromTable } from "../src/operations/utils/computedColumns";
import { unifiedAugmentationSchema } from "../src/types/customSchema";
import { unifiedAugmentationsSyncSchema } from "@repo/types";
import { Kysely, PostgresDialect } from "kysely";

describe("column rename augmentations", () => {
  test("hash is deterministic regardless of rename ordering", () => {
    const ordered = computeAugmentationsHash({
      relations: [],
      computedColumns: [],
      columnConversions: [],
      columnRenames: [
        { table: "users", dbName: "first_name", semanticName: "firstName" },
        { table: "users", dbName: "last_name", semanticName: "lastName" },
      ],
    });

    const reversed = computeAugmentationsHash({
      relations: [],
      computedColumns: [],
      columnConversions: [],
      columnRenames: [
        { table: "users", dbName: "last_name", semanticName: "lastName" },
        { table: "users", dbName: "first_name", semanticName: "firstName" },
      ],
    });

    expect(ordered).toBe(reversed);
  });

  test("hash changes when rename semantic name changes", () => {
    const before = computeAugmentationsHash({
      relations: [],
      computedColumns: [],
      columnConversions: [],
      columnRenames: [
        { table: "users", dbName: "first_name", semanticName: "firstName" },
      ],
    });

    const after = computeAugmentationsHash({
      relations: [],
      computedColumns: [],
      columnConversions: [],
      columnRenames: [
        { table: "users", dbName: "first_name", semanticName: "givenName" },
      ],
    });

    expect(before).not.toBe(after);
  });

  test("getColumnFromTable accepts semantic and db names", () => {
    const schema = {
      tables: [
        {
          name: "users",
          columns: [{ name: "first_name", type: "string" }],
          columnRenameMap: {
            semanticToDb: { firstName: "first_name" },
            dbToSemantic: { first_name: "firstName" },
          },
        },
      ],
    };
    const db = new Kysely<any>({
      dialect: new PostgresDialect({ pool: {} as any }),
    });

    const semanticSql = db
      .selectFrom("users")
      .select(
        getColumnFromTable({
          tableName: "users",
          columnName: "firstName",
          schema: schema as any,
          dialect: "postgresql",
        }).as("value"),
      )
      .compile().sql;
    expect(semanticSql).toContain(`"users"."first_name"`);

    const dbNameSql = db
      .selectFrom("users")
      .select(
        getColumnFromTable({
          tableName: "users",
          columnName: "first_name",
          schema: schema as any,
          dialect: "postgresql",
        }).as("value"),
      )
      .compile().sql;
    expect(dbNameSql).toContain(`"users"."first_name"`);
  });

  test("unified augmentation schema defaults missing columnRenames to empty", () => {
    const parsed = unifiedAugmentationSchema.parse({
      relations: [],
      computedColumns: [],
      columnConversions: [],
    });

    expect(parsed.columnRenames).toEqual([]);
  });

  test("shared sync schema defaults missing columnRenames to empty", () => {
    const parsed = unifiedAugmentationsSyncSchema.parse({
      updatedAt: "2026-02-20T00:00:00.000Z",
      hash: "abc123",
      relations: [],
      computedColumns: [],
      columnConversions: [],
    });

    expect(parsed.columnRenames).toEqual([]);
  });
});
