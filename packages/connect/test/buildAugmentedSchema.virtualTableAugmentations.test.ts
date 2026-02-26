// @ts-nocheck

describe("buildAugmentedSchema virtual table augmentations", () => {
  afterEach(() => {
    jest.resetModules();
    jest.clearAllMocks();
  });

  test("applies computed columns and conversions to virtual tables", async () => {
    const getCachedSchemaMock = jest.fn().mockResolvedValue({
      tables: [
        {
          name: "orders",
          columns: [{ name: "amount", type: "string" }],
          relations: [],
          computedColumns: [],
          columnConversions: [],
        },
      ],
      databaseSchemas: null,
    });

    const readUnifiedAugmentationMock = jest.fn().mockResolvedValue({
      relations: [],
      computedColumns: [
        {
          table: "recent_orders",
          name: "amount_copy",
          ast: { type: "column", name: "amount" },
          selected: true,
        },
      ],
      columnConversions: [
        {
          table: "recent_orders",
          column: "amount",
          ast: {
            type: "cast",
            as: "number",
            expression: { type: "column", name: "amount" },
          },
          type: "number",
          selected: true,
        },
      ],
      columnRenames: [],
      virtualTables: [
        {
          name: "recent_orders",
          dialect: "postgresql",
          sql: "SELECT amount FROM orders",
          selected: true,
          columns: [
            {
              sourceName: "amount",
              name: "amount",
              type: "string",
              selected: true,
            },
          ],
          relations: [],
        },
      ],
    });

    const warnMock = jest.fn();
    const debugMock = jest.fn();

    await jest.unstable_mockModule("~/util/schemaCache", () => ({
      getCachedSchema: getCachedSchemaMock,
    }));
    await jest.unstable_mockModule("~/util/schemaAugmentationStore", () => ({
      readUnifiedAugmentation: readUnifiedAugmentationMock,
    }));
    await jest.unstable_mockModule("~/util/logger", () => ({
      logger: {
        warn: warnMock,
        debug: debugMock,
      },
    }));

    const { buildAugmentedSchema } = await import("~/util/buildAugmentedSchema");
    const schema = await buildAugmentedSchema();

    const recentOrders = schema.tables.find((table: any) => table.name === "recent_orders");
    expect(recentOrders).toBeDefined();
    expect(recentOrders.columns).toEqual([
      {
        name: "amount",
        type: "number",
      },
    ]);
    expect(recentOrders.computedColumns).toEqual([
      {
        name: "amount_copy",
        ast: { type: "column", name: "amount" },
        type: null,
        unit: null,
        notes: null,
      },
    ]);
    expect(recentOrders.columnConversions).toEqual([
      {
        column: "amount",
        ast: {
          type: "cast",
          as: "number",
          expression: { type: "column", name: "amount" },
        },
        type: "number",
      },
    ]);
    expect(warnMock).not.toHaveBeenCalledWith(
      expect.objectContaining({ table: "recent_orders" }),
      "Augmented schema - computed column references unknown table",
    );
  });
});
