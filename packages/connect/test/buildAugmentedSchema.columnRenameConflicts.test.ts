// @ts-nocheck

describe("buildAugmentedSchema column rename conflicts", () => {
  afterEach(() => {
    jest.resetModules();
    jest.clearAllMocks();
  });

  test("skips rename when semantic name conflicts with computed column", async () => {
    const getCachedSchemaMock = jest.fn().mockResolvedValue({
      tables: [
        {
          name: "users",
          columns: [{ name: "first_name", type: "string" }],
          computedColumns: [
            {
              name: "fullName",
              type: "string",
              ast: {
                type: "column",
                name: "first_name",
              },
            },
          ],
          relations: [],
        },
      ],
      databaseSchemas: null,
    });

    const readUnifiedAugmentationMock = jest.fn().mockResolvedValue({
      relations: [],
      computedColumns: [],
      columnConversions: [],
      columnRenames: [
        {
          table: "users",
          dbName: "first_name",
          semanticName: "fullName",
        },
      ],
    });

    const warnMock = jest.fn();

    await jest.unstable_mockModule("~/util/schemaCache", () => ({
      getCachedSchema: getCachedSchemaMock,
    }));
    await jest.unstable_mockModule("~/util/schemaAugmentationStore", () => ({
      readUnifiedAugmentation: readUnifiedAugmentationMock,
    }));
    await jest.unstable_mockModule("~/util/logger", () => ({
      logger: {
        warn: warnMock,
      },
    }));

    const { buildAugmentedSchema } = await import("~/util/buildAugmentedSchema");
    const schema = await buildAugmentedSchema();

    const users = schema.tables.find((table: any) => table.name === "users");
    expect(users).toBeDefined();
    expect(users.columns[0]).toEqual({
      name: "first_name",
      type: "string",
    });
    expect(users.columns[0].semanticName).toBeUndefined();
    expect(users.columnRenameMap?.semanticToDb ?? {}).toEqual({});
    expect(users.columnRenameMap?.dbToSemantic ?? {}).toEqual({});
    expect(warnMock).toHaveBeenCalledWith(
      {
        table: "users",
        semanticName: "fullName",
        dbName: "first_name",
      },
      "Augmented schema - semantic rename conflicts with computed column name",
    );
  });
});
