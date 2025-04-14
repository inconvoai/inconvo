import { QuerySchema } from "~/types/querySchema";
import { count } from "~/operations/count";
import { getDb } from "~/dbConnection";

test("How many lineitems are there with a profit of over $800", async () => {
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
          gt: 800,
        },
      },
    ],
    operation: "count",
    operationParameters: {
      columns: ["_unique_key", "profit_"],
    },
  };

  const parsedQuery = QuerySchema.parse(iql);
  const db = await getDb();
  const response = await count(db, parsedQuery);

  expect(response).toEqual({
    _count: {
      _unique_key: 18,
      profit_: 18,
    },
  });
});
