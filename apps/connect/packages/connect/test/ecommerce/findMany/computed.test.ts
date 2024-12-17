import { getPrismaClient } from "~/prismaClient";
import { QuerySchema } from "~/types/querySchema";
import { findMany } from "~/operations/findMany";

test("What are the revenues for our most profitable orders and what products were sold?", async () => {
  const iql = {
    table: "fct_order",
    computedColumns: [
      {
        name: "profit",
        ast: {
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
                  name: "ORDER_PRODUCT_GROSS_REVENUE",
                },
                {
                  mathjs: "SymbolNode",
                  name: "ORDER_PRODUCT_TAX",
                },
              ],
            },
            {
              mathjs: "SymbolNode",
              name: "ORDER_PRODUCT_COGS",
            },
          ],
        },
        type: "Decimal",
      },
    ],
    whereAndArray: [],
    operation: "findMany",
    operationParameters: {
      columns: {
        fct_order: ["ORDER_PRODUCT_GROSS_REVENUE"],
        "fct_order.fct_order_lineitem.dim_product": ["PRODUCT_NAME"],
      },
      orderBy: {
        column: "profit",
        direction: "desc",
      },
      limit: 5,
    },
  };

  const prisma = getPrismaClient();
  const parsedQuery = QuerySchema.parse(iql);
  const response = await findMany(prisma, parsedQuery);

  const answer = [
    {
      ORDER_PRODUCT_GROSS_REVENUE: 1449,
      fct_order_lineitem: [
        {
          dim_product: {
            PRODUCT_NAME: "Giraffe Toy",
          },
        },
      ],
      profit: 967,
    },
    {
      ORDER_PRODUCT_GROSS_REVENUE: 683,
      fct_order_lineitem: [
        {
          dim_product: {
            PRODUCT_NAME: "Giraffe Toy",
          },
        },
      ],
      profit: 532,
    },
    {
      ORDER_PRODUCT_GROSS_REVENUE: 1078,
      fct_order_lineitem: [],
      profit: 497,
    },
    {
      ORDER_PRODUCT_GROSS_REVENUE: 682,
      fct_order_lineitem: [
        {
          dim_product: {
            PRODUCT_NAME: "Rope and Ball Toy",
          },
        },
      ],
      profit: 476,
    },
    {
      ORDER_PRODUCT_GROSS_REVENUE: 580,
      fct_order_lineitem: [],
      profit: 454,
    },
  ];

  const keysToCompare = Object.keys(answer[0]);
  const filteredResponse = response.map(
    (item: { [s: string]: unknown } | ArrayLike<unknown>) =>
      Object.fromEntries(
        Object.entries(item).filter(([key]) => keysToCompare.includes(key))
      )
  );

  const filteredAnswer = answer.map((item) =>
    Object.fromEntries(
      Object.entries(item).filter(([key]) => keysToCompare.includes(key))
    )
  );

  expect(filteredResponse).toEqual(filteredAnswer);
});
