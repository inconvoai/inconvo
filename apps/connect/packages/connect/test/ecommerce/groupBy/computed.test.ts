import { QuerySchema } from "~/types/querySchema";
import { groupBy } from "~/operations/groupBy";
import { getDb } from "~/dbConnection";

test("What is the product with the highest total profit where the profit on sales of that product were over $100", async () => {
  const iql = {
    table: "fct_order_lineitem",
    computedColumns: [
      {
        name: "profit_",
        ast: {
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
      joins: [
        {
          table: "dim_product",
          joinPath: "fct_order_lineitem.dim_product",
          joinType: "inner",
        },
      ],
      groupBy: [
        { type: "column", column: "fct_order_lineitem.product_key" },
        { type: "column", column: "dim_product.PRODUCT_NAME" },
      ],
      count: null,
      sum: ["fct_order_lineitem.profit_"],
      avg: null,
      min: null,
      max: null,
      orderBy: {
        type: "aggregate",
        function: "sum",
        column: "fct_order_lineitem.profit_",
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
      "fct_order_lineitem.product_key": "shopify_31443282067544",
      _sum: {
        "fct_order_lineitem.profit_": 205770.5833333336,
      },
      "dim_product.PRODUCT_NAME": "Tiger Toy",
    },
  ]);
});
