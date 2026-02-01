import type { Kysely } from "kysely";

export type MultiHopFixture = {
  baseTable: string;
  midTable: string;
  endTable: string;
  baseColumns: {
    id: string;
    primaryMidId: string;
    secondaryMidId: string;
  };
  midColumns: {
    id: string;
    endId: string;
  };
  endColumns: {
    id: string;
    label: string;
  };
};

export const setupMultiHopFixture = async (
  db: Kysely<any>,
): Promise<MultiHopFixture> => {
  const fixture: MultiHopFixture = {
    baseTable: "inconvo_join_base",
    midTable: "inconvo_join_mid",
    endTable: "inconvo_join_end",
    baseColumns: {
      id: "id",
      primaryMidId: "primary_mid_id",
      secondaryMidId: "secondary_mid_id",
    },
    midColumns: {
      id: "id",
      endId: "end_id",
    },
    endColumns: {
      id: "id",
      label: "label",
    },
  };

  await db.schema.dropTable(fixture.baseTable).ifExists().execute();
  await db.schema.dropTable(fixture.midTable).ifExists().execute();
  await db.schema.dropTable(fixture.endTable).ifExists().execute();

  await db.schema
    .createTable(fixture.endTable)
    .addColumn(fixture.endColumns.id, "integer", (col) => col.primaryKey())
    .addColumn(fixture.endColumns.label, "varchar(255)")
    .execute();

  await db.schema
    .createTable(fixture.midTable)
    .addColumn(fixture.midColumns.id, "integer", (col) => col.primaryKey())
    .addColumn(fixture.midColumns.endId, "integer")
    .execute();

  await db.schema
    .createTable(fixture.baseTable)
    .addColumn(fixture.baseColumns.id, "integer", (col) => col.primaryKey())
    .addColumn(fixture.baseColumns.primaryMidId, "integer")
    .addColumn(fixture.baseColumns.secondaryMidId, "integer")
    .execute();

  await db
    .insertInto(fixture.endTable)
    .values([
      { id: 1, label: "end_a" },
      { id: 2, label: "end_b" },
    ])
    .execute();

  await db
    .insertInto(fixture.midTable)
    .values([
      { id: 10, end_id: 1 },
      { id: 20, end_id: 2 },
    ])
    .execute();

  await db
    .insertInto(fixture.baseTable)
    .values([
      { id: 100, primary_mid_id: 10, secondary_mid_id: 20 },
    ])
    .execute();

  return fixture;
};

export const teardownMultiHopFixture = async (
  db: Kysely<any>,
  fixture: MultiHopFixture,
) => {
  await db.schema.dropTable(fixture.baseTable).ifExists().execute();
  await db.schema.dropTable(fixture.midTable).ifExists().execute();
  await db.schema.dropTable(fixture.endTable).ifExists().execute();
};
