import { QuerySchema } from "~/types/querySchema";
import { findMany } from "~/operations/findMany";
import { getDb } from "~/dbConnection";

test("What are the revenues for our most profitable orders and what products were sold?", async () => {
  const iql = {
    table: "fct_order",
    computedColumns: [
      {
        name: "profit_",
        ast: {
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
    whereAndArray: [],
    operation: "findMany",
    operationParameters: {
      columns: {
        fct_order: ["ORDER_PRODUCT_GROSS_REVENUE", "profit_"],
        "fct_order.fct_order_lineitems.dim_product": ["PRODUCT_NAME"],
      },
      orderBy: {
        column: "profit_",
        direction: "desc",
      },
      limit: 5,
    },
  };

  const parsedQuery = QuerySchema.parse(iql);
  const db = await getDb();
  const response = await findMany(db, parsedQuery);

  const answer = [
    {
      ORDER_PRODUCT_GROSS_REVENUE: 1449,
      fct_order_lineitems: [
        {
          dim_product: {
            PRODUCT_NAME: "Giraffe Toy",
          },
        },
      ],
      profit_: 967,
    },
    {
      ORDER_PRODUCT_GROSS_REVENUE: 683,
      fct_order_lineitems: [
        {
          dim_product: {
            PRODUCT_NAME: "Giraffe Toy",
          },
        },
      ],
      profit_: 532,
    },
    {
      ORDER_PRODUCT_GROSS_REVENUE: 1078,
      fct_order_lineitems: null,
      profit_: 497,
    },
    {
      ORDER_PRODUCT_GROSS_REVENUE: 682,
      fct_order_lineitems: [
        {
          dim_product: {
            PRODUCT_NAME: "Rope and Ball Toy",
          },
        },
      ],
      profit_: 476,
    },
    {
      ORDER_PRODUCT_GROSS_REVENUE: 580,
      fct_order_lineitems: null,
      profit_: 454,
    },
  ];

  expect(response).toEqual(answer);
});
