import { QuerySchema } from "~/types/querySchema";
import { countRelations } from "~/operations/countRelations";
import { getDb } from "~/dbConnection";

test("What was the order with the most lineitems with an order profit of over $100", async () => {
  const iql = {
    table: "fct_order",
    computedColumns: [
      {
        name: "profit_",
        expression: {
          type: "operation",
          operator: "+",
          operands: [
            {
              type: "operation",
              operator: "+",
              operands: [
                {
                  type: "column",
                  name: "ORDER_PRODUCT_GROSS_REVENUE",
                },
                {
                  type: "column",
                  name: "ORDER_PRODUCT_TAX",
                },
              ],
            },
            {
              type: "column",
              name: "ORDER_PRODUCT_COGS",
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
    operation: "countRelations",
    operationParameters: {
      columns: ["_unique_key", "store_key", "profit_"],
      relationsToCount: [
        {
          name: "fct_order_lineitems",
          distinct: null,
        },
      ],
      orderBy: {
        relation: "fct_order_lineitems",
        direction: "desc",
      },
      limit: 1,
    },
  };

  const parsedQuery = QuerySchema.parse(iql);
  const db = await getDb();
  const response = await countRelations(db, parsedQuery);

  const answer = [
    {
      _unique_key: "5484008702256",
      store_key: "25824624728",
      fct_order_lineitemsCount: 3,
      profit_: 163,
    },
  ];

  expect(response).toEqual(answer);
}, 10000);
