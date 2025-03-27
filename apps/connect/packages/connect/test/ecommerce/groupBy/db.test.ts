import { QuerySchema } from "~/types/querySchema";
import { groupBy } from "~/operations/groupBy";
import { getDb } from "~/dbConnection";

test("What are my highest revenue generating products?", async () => {
  const iql = {
    table: "fct_order_lineitem",
    whereAndArray: [],
    operation: "groupBy",
    operationParameters: {
      groupBy: [
        {
          column: "product_key",
          join: {
            dim_product: "PRODUCT_NAME",
          },
        },
      ],
      count: null,
      sum: {
        columns: ["ORDER_LINEITEM_PRODUCT_GROSS_REVENUE"],
      },
      min: null,
      max: null,
      avg: null,
      orderBy: {
        function: "sum",
        column: "ORDER_LINEITEM_PRODUCT_GROSS_REVENUE",
        direction: "desc",
      },
      limit: 10,
    },
  };

  const parsedQuery = QuerySchema.parse(iql);
  const db = await getDb();
  const response = await groupBy(db, parsedQuery);

  expect(response).toEqual([
    {
      _sum: {
        ORDER_LINEITEM_PRODUCT_GROSS_REVENUE: 306747,
      },
      product_key: "shopify_31443282067544",
      PRODUCT_NAME: "Tiger Toy",
    },
    {
      _sum: {
        ORDER_LINEITEM_PRODUCT_GROSS_REVENUE: 187204,
      },
      product_key: "shopify_44110720696624",
      PRODUCT_NAME: "Rope and Ball Toy",
    },
    {
      _sum: {
        ORDER_LINEITEM_PRODUCT_GROSS_REVENUE: 185895,
      },
      product_key: "shopify_46665181692208",
      PRODUCT_NAME: "Elephant Toy",
    },
    {
      _sum: {
        ORDER_LINEITEM_PRODUCT_GROSS_REVENUE: 132908,
      },
      product_key: "shopify_46988847415600",
      PRODUCT_NAME: "Rope and Ball Toy",
    },
    {
      _sum: {
        ORDER_LINEITEM_PRODUCT_GROSS_REVENUE: 125428,
      },
      product_key: "shopify_40077336641624",
      PRODUCT_NAME: "Giraffe Toy",
    },
    {
      _sum: {
        ORDER_LINEITEM_PRODUCT_GROSS_REVENUE: 114332,
      },
      product_key: "shopify_46988783780144",
      PRODUCT_NAME: "Tiger Toy",
    },
    {
      _sum: {
        ORDER_LINEITEM_PRODUCT_GROSS_REVENUE: 59458,
      },
      product_key: "shopify_30150991413336",
      PRODUCT_NAME: "Tiger Toy",
    },
    {
      _sum: {
        ORDER_LINEITEM_PRODUCT_GROSS_REVENUE: 54851,
      },
      product_key: "shopify_32103211303000",
      PRODUCT_NAME: "Reindeer Toy",
    },
    {
      _sum: {
        ORDER_LINEITEM_PRODUCT_GROSS_REVENUE: 49596,
      },
      product_key: "shopify_30151925137496",
      PRODUCT_NAME: "Shark Toy",
    },
    {
      _sum: {
        ORDER_LINEITEM_PRODUCT_GROSS_REVENUE: 45329,
      },
      product_key: "shopify_40077336608856",
      PRODUCT_NAME: "Giraffe Toy",
    },
  ]);
});
