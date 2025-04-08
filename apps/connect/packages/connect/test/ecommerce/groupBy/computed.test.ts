import { QuerySchema } from "~/types/querySchema";
import { groupBy } from "~/operations/groupBy";
import { getDb } from "~/dbConnection";

test("What is the product with the highest total profit where the profit on sales of that product were over $100", async () => {
  const iql = {
    table: "fct_order_lineitem",
    computedColumns: [
      {
        name: "profit_",
        expression: {
          type: "operation",
          operator: "*",
          operands: [
            {
              type: "column",
              name: "num_orders",
            },
            {
              type: "operation",
              operator: "+",
              operands: [
                {
                  type: "operation",
                  operator: "+",
                  operands: [
                    {
                      type: "column",
                      name: "ORDER_LINEITEM_PRODUCT_GROSS_REVENUE",
                    },
                    {
                      type: "column",
                      name: "ORDER_LINEITEM_PRODUCT_TAX",
                    },
                  ],
                },
                {
                  type: "column",
                  name: "ORDER_LINEITEM_PRODUCT_COGS",
                },
              ],
            },
          ],
        },
        type: "number",
      },
    ],
    whereAndArray: [
      {
        profit_: {
          gt: 100,
        },
      },
    ],
    operation: "groupBy",
    operationParameters: {
      groupBy: [
        {
          column: "product_key",
          join: {
            dim_product: "PRODUCT_NAME",
          },
        },
      ],
      count: null,
      sum: {
        columns: ["profit_"],
      },
      avg: null,
      min: null,
      max: null,
      orderBy: {
        function: "sum",
        column: "profit_",
        direction: "desc",
      },
      limit: 1,
    },
  };

  const parsedQuery = QuerySchema.parse(iql);
  const db = await getDb();
  const response = await groupBy(db, parsedQuery);

  expect(response).toEqual([
    {
      product_key: "shopify_31443282067544",
      _sum: {
        profit_: 205770.5833333336,
      },
      PRODUCT_NAME: "Tiger Toy",
    },
  ]);
});
