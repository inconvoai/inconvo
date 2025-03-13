import { getPrismaClient } from "~/prismaClient";
import { QuerySchema } from "~/types/querySchema";
import { groupBy } from "~/operations/groupBy";

test("What is the product with the highest total profit where the profit on sales of that product were over $100", async () => {
  const iql = {
    table: "fct_order_lineitem",
    computedColumns: [
      {
        name: "profit",
        ast: {
          mathjs: "OperatorNode",
          op: "*",
          fn: "multiply",
          args: [
            {
              mathjs: "SymbolNode",
              name: "num_orders",
            },
            {
              mathjs: "ParenthesisNode",
              content: {
                mathjs: "OperatorNode",
                op: "+",
                fn: "add",
                args: [
                  {
                    mathjs: "OperatorNode",
                    op: "+",
                    fn: "add",
                    args: [
                      {
                        mathjs: "SymbolNode",
                        name: "ORDER_LINEITEM_PRODUCT_GROSS_REVENUE",
                      },
                      {
                        mathjs: "SymbolNode",
                        name: "ORDER_LINEITEM_PRODUCT_TAX",
                      },
                    ],
                  },
                  {
                    mathjs: "SymbolNode",
                    name: "ORDER_LINEITEM_PRODUCT_COGS",
                  },
                ],
              },
            },
          ],
        },
        type: "Decimal",
      },
    ],
    whereAndArray: [
      {
        profit: {
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
        columns: ["profit"],
      },
      avg: null,
      min: null,
      max: null,
      orderBy: {
        function: "sum",
        column: "profit",
        direction: "desc",
      },
      limit: 1,
    },
  };

  const prisma = getPrismaClient();
  const parsedQuery = QuerySchema.parse(iql);
  const response = await groupBy(prisma, parsedQuery);

  expect(response).toEqual([
    {
      product_key: "shopify_31443282067544",
      _sum: {
        profit: 205770.5833333336,
      },
      PRODUCT_NAME: "Tiger Toy",
    },
  ]);
});
