import { getPrismaClient } from "~/prismaClient";
import { QuerySchema } from "~/types/querySchema";
import { aggregate } from "~/operations/aggregate";

test("What is our average profit on a lineitem?", async () => {
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
    whereAndArray: [],
    operation: "aggregate",
    operationParameters: {
      min: ["profit"],
      max: ["profit"],
      avg: ["profit"],
      sum: ["profit"],
      count: ["profit"],
      median: null,
    },
  };

  const prisma = getPrismaClient();
  const parsedQuery = QuerySchema.parse(iql);
  const response = await aggregate(prisma, parsedQuery);

  expect(response).toEqual({
    _avg: {
      profit: 119.34086097368841,
    },
    _sum: {
      profit: 1002821.2547619037,
    },
    _min: {
      profit: -92,
    },
    _max: {
      profit: 942,
    },
    _count: {
      profit: 8403,
    },
  });
});

test("How many times have we made more than $900 profit on a lineitem?", async () => {
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
          gt: 900,
        },
      },
    ],
    operation: "aggregate",
    operationParameters: {
      min: ["profit"],
      max: ["profit"],
      avg: ["profit"],
      sum: ["profit"],
      count: ["profit"],
      median: null,
    },
  };

  const prisma = getPrismaClient();
  const parsedQuery = QuerySchema.parse(iql);
  const response = await aggregate(prisma, parsedQuery);

  expect(response).toEqual({
    _avg: {
      profit: 936.3333333333334,
    },
    _sum: {
      profit: 2809,
    },
    _min: {
      profit: 925,
    },
    _max: {
      profit: 942,
    },
    _count: {
      profit: 3,
    },
  });
});
