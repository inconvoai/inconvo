import { getPrismaClient } from "~/prismaClient";
import { QuerySchema } from "~/types/querySchema";
import { aggregateByDateInterval } from "~/operations/aggregateByDateInterval";

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
    operation: "aggregateByDateInterval",
    operationParameters: {
      interval: "month",
      dateColumn: "ORDER_TIMESTAMP",
      aggregateColumn: "ORDER_TIMESTAMP",
      aggregationType: "count",
    },
  };

  const prisma = getPrismaClient();
  const parsedQuery = QuerySchema.parse(iql);
  const response = await aggregateByDateInterval(prisma, parsedQuery);

  expect(response).toEqual({
    "2024-09-01": { count: 33 },
    "2024-10-01": { count: 75 },
    "2024-11-01": { count: 66 },
    "2024-12-01": { count: 48 },
    "2025-01-01": { count: 42 },
    "2025-02-01": { count: 68 },
    "2025-03-01": { count: 89 },
    "2025-04-01": { count: 55 },
    "2025-05-01": { count: 58 },
    "2025-06-01": { count: 62 },
    "2025-07-01": { count: 72 },
    "2025-08-01": { count: 32 },
  });
});
