import { getPrismaClient } from "~/prismaClient";
import { QuerySchema } from "~/types/querySchema";
import { aggregate } from "~/operations/aggregate";

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

  const prisma = getPrismaClient();
  const parsedQuery = QuerySchema.parse(iql);
  const response = await aggregate(prisma, parsedQuery);

  expect(response).toEqual({
    _avg: {
      ORDER_PRODUCT_GROSS_REVENUE: 129.62413280475718,
    },
    _sum: {
      ORDER_PRODUCT_GROSS_REVENUE: 2092652,
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

  const prisma = getPrismaClient();
  const parsedQuery = QuerySchema.parse(iql);
  const response = await aggregate(prisma, parsedQuery);

  expect(response).toEqual({
    _avg: {
      ORDER_PRODUCT_GROSS_REVENUE: 215.8875,
    },
    _sum: {
      ORDER_PRODUCT_GROSS_REVENUE: 1761642,
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
