import { QuerySchema } from "~/types/querySchema";
import { countByTemporalComponent } from "~/operations/countByTemporalComponent";

test.skip("How many lineitems have been sold per day of the week where the profit was over $500?", async () => {
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
    operation: "countByTemporalComponent",
    operationParameters: {
      component: "Day",
      dateColumn: "ORDER_TIMESTAMP",
    },
  };

  const parsedQuery = QuerySchema.parse(iql);
  const response = await countByTemporalComponent(parsedQuery);

  expect(response).toEqual({
    Wednesday: 105,
    Thursday: 120,
    Sunday: 92,
    Tuesday: 101,
    Friday: 121,
    Saturday: 98,
    Monday: 63,
  });
});
