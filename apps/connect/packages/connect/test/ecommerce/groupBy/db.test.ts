import { QuerySchema } from "~/types/querySchema";
import { groupBy } from "~/operations/groupBy";
import { getDb } from "~/dbConnection";

test("What are my highest revenue generating products?", async () => {
  const iql = {
    table: "fct_order_lineitem",
    whereAndArray: [],
    operation: "groupBy",
    operationParameters: {
      joins: [
        {
          table: "dim_product",
          joinPath: "fct_order_lineitem.dim_product",
          joinType: "inner",
        },
      ],
      groupBy: ["fct_order_lineitem.product_key", "dim_product.PRODUCT_NAME"],
      count: null,
      sum: {
        columns: ["fct_order_lineitem.ORDER_LINEITEM_PRODUCT_GROSS_REVENUE"],
      },
      min: null,
      max: null,
      avg: null,
      orderBy: {
        function: "sum",
        column: "fct_order_lineitem.ORDER_LINEITEM_PRODUCT_GROSS_REVENUE",
        direction: "desc",
      },
      limit: 10,
    },
  };

  const parsedQuery = QuerySchema.parse(iql);
  const db = await getDb();
  const response = await groupBy(db, parsedQuery);

  console.log("response", response);

  expect(response).toEqual([
    {
      _sum: {
        "fct_order_lineitem.ORDER_LINEITEM_PRODUCT_GROSS_REVENUE": 306747,
      },
      "fct_order_lineitem.product_key": "shopify_31443282067544",
      "dim_product.PRODUCT_NAME": "Tiger Toy",
    },
    {
      _sum: {
        "fct_order_lineitem.ORDER_LINEITEM_PRODUCT_GROSS_REVENUE": 187204,
      },
      "fct_order_lineitem.product_key": "shopify_44110720696624",
      "dim_product.PRODUCT_NAME": "Rope and Ball Toy",
    },
    {
      _sum: {
        "fct_order_lineitem.ORDER_LINEITEM_PRODUCT_GROSS_REVENUE": 185895,
      },
      "fct_order_lineitem.product_key": "shopify_46665181692208",
      "dim_product.PRODUCT_NAME": "Elephant Toy",
    },
    {
      _sum: {
        "fct_order_lineitem.ORDER_LINEITEM_PRODUCT_GROSS_REVENUE": 132908,
      },
      "fct_order_lineitem.product_key": "shopify_46988847415600",
      "dim_product.PRODUCT_NAME": "Rope and Ball Toy",
    },
    {
      _sum: {
        "fct_order_lineitem.ORDER_LINEITEM_PRODUCT_GROSS_REVENUE": 125428,
      },
      "fct_order_lineitem.product_key": "shopify_40077336641624",
      "dim_product.PRODUCT_NAME": "Giraffe Toy",
    },
    {
      _sum: {
        "fct_order_lineitem.ORDER_LINEITEM_PRODUCT_GROSS_REVENUE": 114332,
      },
      "fct_order_lineitem.product_key": "shopify_46988783780144",
      "dim_product.PRODUCT_NAME": "Tiger Toy",
    },
    {
      _sum: {
        "fct_order_lineitem.ORDER_LINEITEM_PRODUCT_GROSS_REVENUE": 59458,
      },
      "fct_order_lineitem.product_key": "shopify_30150991413336",
      "dim_product.PRODUCT_NAME": "Tiger Toy",
    },
    {
      _sum: {
        "fct_order_lineitem.ORDER_LINEITEM_PRODUCT_GROSS_REVENUE": 54851,
      },
      "fct_order_lineitem.product_key": "shopify_32103211303000",
      "dim_product.PRODUCT_NAME": "Reindeer Toy",
    },
    {
      _sum: {
        "fct_order_lineitem.ORDER_LINEITEM_PRODUCT_GROSS_REVENUE": 49596,
      },
      "fct_order_lineitem.product_key": "shopify_30151925137496",
      "dim_product.PRODUCT_NAME": "Shark Toy",
    },
    {
      _sum: {
        "fct_order_lineitem.ORDER_LINEITEM_PRODUCT_GROSS_REVENUE": 45329,
      },
      "fct_order_lineitem.product_key": "shopify_40077336608856",
      "dim_product.PRODUCT_NAME": "Giraffe Toy",
    },
  ]);
});
