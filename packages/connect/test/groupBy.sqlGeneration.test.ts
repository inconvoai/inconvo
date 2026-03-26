// @ts-nocheck
import { Kysely, PostgresDialect } from "kysely";

const schema = {
  tables: [
    {
      name: "orders",
      schema: "bench_debugging_pretext_1_wygmjicy",
      columns: [
        { name: "id", type: "number" },
        { name: "product_id", type: "number" },
        { name: "quantity", type: "number" },
        { name: "total", type: "number" },
      ],
    },
    {
      name: "products",
      schema: "bench_debugging_pretext_1_wygmjicy",
      columns: [
        { name: "id", type: "number" },
        { name: "title", type: "string" },
        { name: "price", type: "number" },
      ],
    },
  ],
  databaseSchemas: ["bench_debugging_pretext_1_wygmjicy", "public"],
};

describe("groupBy SQL generation with explicit tableSchema joins", () => {
  let groupByOperation: (typeof import("~/operations/groupBy"))["groupBy"];
  const executeWithLoggingMock = jest.fn(async (query: any) => ({
    rows: [],
    compiled: query.compile(),
  }));

  beforeAll(async () => {
    await jest.unstable_mockModule("~/operations/utils/executeWithLogging", () => ({
      executeWithLogging: executeWithLoggingMock,
    }));
    ({ groupBy: groupByOperation } = await import("~/operations/groupBy"));
  });

  beforeEach(() => {
    executeWithLoggingMock.mockClear();
  });

  it("references the base table by its FROM-clause name in JOIN conditions", async () => {
    const db = new Kysely<any>({
      dialect: new PostgresDialect({ pool: {} as any }),
    });

    const response = await groupByOperation(
      db,
      {
        table: "orders",
        tableSchema: "public",
        tableConditions: null,
        whereAndArray: [],
        operation: "groupBy",
        operationParameters: {
          avg: null,
          count: ["orders.id"],
          countDistinct: null,
          groupBy: [
            {
              alias: "product_title",
              column: "products.title",
              type: "column",
            },
          ],
          having: null,
          joins: [
            {
              joinType: "inner",
              name: "orders.product",
              path: [
                {
                  source: ["orders.product_id"],
                  target: ["products.id"],
                },
              ],
              table: "products",
            },
          ],
          limit: 1,
          max: null,
          min: null,
          orderBy: {
            column: "orders.quantity",
            direction: "desc",
            function: "sum",
            type: "aggregate",
          },
          sum: ["orders.quantity", "orders.total"],
        },
      } as any,
      {
        schema,
        dialect: "postgresql",
      },
    );

    expect(response.query.sql).toContain('from "public"."orders"');
    expect(response.query.sql).toContain(
      'inner join "bench_debugging_pretext_1_wygmjicy"."products"',
    );
    expect(response.query.sql).toContain('"orders"."product_id"');
    expect(response.query.sql).not.toContain(
      '"bench_debugging_pretext_1_wygmjicy"."orders"."product_id"',
    );
  });

  it("resolves custom join aliases in grouped aggregate fields", async () => {
    const db = new Kysely<any>({
      dialect: new PostgresDialect({ pool: {} as any }),
    });

    const response = await groupByOperation(
      db,
      {
        table: "orders",
        tableSchema: "public",
        tableConditions: null,
        whereAndArray: [],
        operation: "groupBy",
        operationParameters: {
          avg: null,
          count: null,
          countDistinct: null,
          groupBy: [
            {
              alias: "product_title",
              column: "product.title",
              type: "column",
            },
          ],
          having: null,
          joins: [
            {
              joinType: "inner",
              name: "product",
              path: [
                {
                  source: ["orders.product_id"],
                  target: ["products.id"],
                },
              ],
              table: "products",
            },
          ],
          limit: 1,
          max: null,
          min: null,
          orderBy: {
            column: "product.price",
            direction: "desc",
            function: "sum",
            type: "aggregate",
          },
          sum: ["product.price"],
        },
      } as any,
      {
        schema,
        dialect: "postgresql",
      },
    );

    expect(response.query.sql).toContain('SUM("products"."price")');
    expect(response.query.sql).not.toContain('SUM("product"."price")');
  });

  it("resolves custom join aliases in questionConditions filters", async () => {
    const db = new Kysely<any>({
      dialect: new PostgresDialect({ pool: {} as any }),
    });

    const response = await groupByOperation(
      db,
      {
        table: "orders",
        tableSchema: "public",
        tableConditions: null,
        whereAndArray: [
          {
            AND: [
              {
                "orders.total": {
                  gt: 0,
                },
              },
              {
                "product.price": {
                  gte: 100,
                },
              },
            ],
          },
        ],
        operation: "groupBy",
        operationParameters: {
          avg: null,
          count: null,
          countDistinct: null,
          groupBy: [
            {
              alias: "product_title",
              column: "product.title",
              type: "column",
            },
          ],
          having: null,
          joins: [
            {
              joinType: "inner",
              name: "product",
              path: [
                {
                  source: ["orders.product_id"],
                  target: ["products.id"],
                },
              ],
              table: "products",
            },
          ],
          limit: 1,
          max: null,
          min: null,
          orderBy: {
            column: "orders.quantity",
            direction: "desc",
            function: "sum",
            type: "aggregate",
          },
          sum: ["orders.total"],
        },
      } as any,
      {
        schema,
        dialect: "postgresql",
      },
    );

    expect(response.query.sql).toContain('"products"."price" >= $2');
    expect(response.query.sql).not.toContain('"product"."price"');
  });
});
