import { QuerySchema } from "~/types/querySchema";
import { findMany } from "~/operations/findMany/index";

test("Which order has generated the most revenue?", async () => {
  const iql = {
    table: "fct_order",
    whereAndArray: [],
    operation: "findMany",
    operationParameters: {
      columns: {
        fct_order: ["_unique_key", "ORDER_PRODUCT_GROSS_REVENUE"],
        "fct_order.fct_order_lineitem.dim_product": ["PRODUCT_NAME"],
      },
      orderBy: {
        column: "ORDER_PRODUCT_GROSS_REVENUE",
        direction: "desc",
      },
      limit: 1,
    },
  };

  const parsedQuery = QuerySchema.parse(iql);
  const response = await findMany(parsedQuery);

  expect(response).toEqual([
    {
      _unique_key: "5942455828784",
      ORDER_PRODUCT_GROSS_REVENUE: 1449,
      fct_order_lineitem: [
        {
          dim_product: {
            PRODUCT_NAME: "Giraffe Toy",
          },
        },
      ],
    },
  ]);
}, 10000);
