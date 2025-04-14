import { QuerySchema } from "~/types/querySchema";
import { countByTemporalComponent } from "~/operations/countByTemporalComponent";
import { getDb } from "~/dbConnection";

test("How many lineitems have been sold per day of the week where the profit was over $500?", async () => {
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
  const db = await getDb();
  const response = await countByTemporalComponent(db, parsedQuery);

  expect(response).toEqual({
    Wednesday: 105,
    Thursday: 123,
    Sunday: 92,
    Tuesday: 100,
    Friday: 118,
    Saturday: 99,
    Monday: 63,
  });
});
