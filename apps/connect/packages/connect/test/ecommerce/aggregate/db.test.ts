import { QuerySchema } from "~/types/querySchema";
import { aggregate } from "~/operations/aggregate";
import { getDb } from "~/dbConnection";

test("What is the most gross revenue we have made on an order?", async () => {
  const iql = {
    table: "fct_order",
    whereAndArray: [],
    operation: "aggregate",
    operationParameters: {
      min: ["ORDER_PRODUCT_GROSS_REVENUE"],
      max: ["ORDER_PRODUCT_GROSS_REVENUE"],
      avg: ["ORDER_PRODUCT_GROSS_REVENUE"],
      sum: ["ORDER_PRODUCT_GROSS_REVENUE"],
      count: ["ORDER_PRODUCT_GROSS_REVENUE"],
      median: null,
    },
  };

  const parsedQuery = QuerySchema.parse(iql);
  const db = await getDb();
  const response = await aggregate(db, parsedQuery);

  expect(response).toEqual({
    _avg: {
      ORDER_PRODUCT_GROSS_REVENUE: "129.6241328047571853",
    },
    _sum: {
      ORDER_PRODUCT_GROSS_REVENUE: "2092652",
    },
    _min: {
      ORDER_PRODUCT_GROSS_REVENUE: 0,
    },
    _max: {
      ORDER_PRODUCT_GROSS_REVENUE: 1449,
    },
    _count: {
      ORDER_PRODUCT_GROSS_REVENUE: 16144,
    },
  });
});

test("What is the most gross revenue we made on an order which was priced over $100?", async () => {
  const iql = {
    table: "fct_order",
    whereAndArray: [
      {
        ORDER_PRODUCT_ORIGINAL_PRICE: {
          gt: 100,
        },
      },
    ],
    operation: "aggregate",
    operationParameters: {
      min: ["ORDER_PRODUCT_GROSS_REVENUE"],
      max: ["ORDER_PRODUCT_GROSS_REVENUE"],
      avg: ["ORDER_PRODUCT_GROSS_REVENUE"],
      sum: ["ORDER_PRODUCT_GROSS_REVENUE"],
      count: ["ORDER_PRODUCT_GROSS_REVENUE"],
      median: null,
    },
  };

  const parsedQuery = QuerySchema.parse(iql);
  const db = await getDb();
  const response = await aggregate(db, parsedQuery);

  expect(response).toEqual({
    _avg: {
      ORDER_PRODUCT_GROSS_REVENUE: "215.8875000000000000",
    },
    _sum: {
      ORDER_PRODUCT_GROSS_REVENUE: "1761642",
    },
    _min: {
      ORDER_PRODUCT_GROSS_REVENUE: 0,
    },
    _max: {
      ORDER_PRODUCT_GROSS_REVENUE: 1449,
    },
    _count: {
      ORDER_PRODUCT_GROSS_REVENUE: 8160,
    },
  });
});
