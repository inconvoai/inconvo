import { QuerySchema } from "~/types/querySchema";
import { aggregateByDateInterval } from "~/operations/aggregateByDateInterval";
import { getDb } from "~/dbConnection";

test("How many lineitems were sold each month which have a profit of over $500", async () => {
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
    operation: "aggregateByDateInterval",
    operationParameters: {
      interval: "month",
      dateColumn: "ORDER_TIMESTAMP",
      aggregateColumn: "ORDER_TIMESTAMP",
      aggregationType: "count",
    },
  };

  const parsedQuery = QuerySchema.parse(iql);
  const db = await getDb();
  const response = await aggregateByDateInterval(db, parsedQuery);

  expect(response).toEqual({
    "2024-09": { count: 33 },
    "2024-10": { count: 75 },
    "2024-11": { count: 66 },
    "2024-12": { count: 48 },
    "2025-01": { count: 42 },
    "2025-02": { count: 68 },
    "2025-03": { count: 89 },
    "2025-04": { count: 55 },
    "2025-05": { count: 58 },
    "2025-06": { count: 62 },
    "2025-07": { count: 72 },
    "2025-08": { count: 32 },
  });
});

test("What was the average profit per month where the profit was over $500", async () => {
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
    operation: "aggregateByDateInterval",
    operationParameters: {
      interval: "month",
      dateColumn: "ORDER_TIMESTAMP",
      aggregateColumn: "profit_",
      aggregationType: "avg",
    },
  };

  const parsedQuery = QuerySchema.parse(iql);
  const db = await getDb();
  const response = await aggregateByDateInterval(db, parsedQuery);

  expect(response).toEqual({
    "2024-09": {
      avg: "572.9090909090909",
    },
    "2024-10": {
      avg: "591.0266666666666",
    },
    "2024-11": {
      avg: "597.6666666666666",
    },
    "2024-12": {
      avg: "627.375",
    },
    "2025-01": {
      avg: "652.3571428571429",
    },
    "2025-02": {
      avg: "620.8823529411765",
    },
    "2025-03": {
      avg: "619.056179775281",
    },
    "2025-04": {
      avg: "610.5818181818182",
    },
    "2025-05": {
      avg: "614.7413793103449",
    },
    "2025-06": {
      avg: "617.1451612903226",
    },
    "2025-07": {
      avg: "634.6666666666666",
    },
    "2025-08": {
      avg: "621.53125",
    },
  });
});
