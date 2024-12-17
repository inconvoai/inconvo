import { getPrismaClient } from "~/prismaClient";
import { QuerySchema } from "~/types/querySchema";
import { countByDateInterval } from "~/operations/countByDateInterval";

test("How many lineitems were sold each month which have a profit of over $500", async () => {
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
          gt: 500,
        },
      },
    ],
    operation: "countByDateInterval",
    operationParameters: {
      interval: "month",
      dateColumn: "ORDER_TIMESTAMP",
    },
  };

  const prisma = getPrismaClient();
  const parsedQuery = QuerySchema.parse(iql);
  const response = await countByDateInterval(prisma, parsedQuery);

  expect(response).toEqual({
    "2024-09-01": 33,
    "2024-10-01": 75,
    "2024-11-01": 66,
    "2024-12-01": 48,
    "2025-01-01": 42,
    "2025-02-01": 68,
    "2025-03-01": 89,
    "2025-04-01": 55,
    "2025-05-01": 58,
    "2025-06-01": 62,
    "2025-07-01": 72,
    "2025-08-01": 32,
  });
});
