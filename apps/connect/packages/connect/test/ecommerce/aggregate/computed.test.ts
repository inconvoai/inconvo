import { QuerySchema } from "~/types/querySchema";
import { aggregate } from "~/operations/aggregate";
import { getDb } from "~/dbConnection";

test("What is our average profit on a lineitem?", async () => {
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
    whereAndArray: [],
    operation: "aggregate",
    operationParameters: {
      min: ["profit_"],
      max: ["profit_"],
      avg: ["profit_"],
      sum: ["profit_"],
      count: ["profit_"],
      median: null,
    },
  };

  const parsedQuery = QuerySchema.parse(iql);
  const db = await getDb();
  const response = await aggregate(db, parsedQuery);

  expect(response).toEqual({
    _avg: {
      profit_: 119.34086097368841,
    },
    _sum: {
      profit_: 1002821.2547619037,
    },
    _min: {
      profit_: -92,
    },
    _max: {
      profit_: 942,
    },
    _count: {
      profit_: 8403,
    },
  });
});

test("How many times have we made more than $900 profit on a lineitem?", async () => {
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
          gt: 900,
        },
      },
    ],
    operation: "aggregate",
    operationParameters: {
      min: ["profit_"],
      max: ["profit_"],
      avg: ["profit_"],
      sum: ["profit_"],
      count: ["profit_"],
      median: null,
    },
  };

  const parsedQuery = QuerySchema.parse(iql);
  const db = await getDb();
  const response = await aggregate(db, parsedQuery);

  expect(response).toEqual({
    _avg: {
      profit_: 936.3333333333334,
    },
    _sum: {
      profit_: 2809,
    },
    _min: {
      profit_: 925,
    },
    _max: {
      profit_: 942,
    },
    _count: {
      profit_: 3,
    },
  });
});
