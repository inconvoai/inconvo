import { QuerySchema } from "~/types/querySchema";
import { findDistinct } from "~/operations/findDistinct";
import { getDb } from "~/dbConnection";

test("Find Unique Channel Keys for an order where profit is greater than 200", async () => {
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
    whereAndArray: [
      {
        profit_: {
          gt: 200,
        },
      },
    ],
    operation: "findDistinct",
    operationParameters: {
      column: "CHANNEL_KEY",
    },
  };

  const parsedQuery = QuerySchema.parse(iql);
  const db = await getDb();
  const response = await findDistinct(db, parsedQuery);

  expect(response.length).toBeLessThanOrEqual(250);
  expect(response.length).toBe(20);

  expect(response).toEqual([
    { CHANNEL_KEY: 101 },
    { CHANNEL_KEY: 1000 },
    { CHANNEL_KEY: 302 },
    { CHANNEL_KEY: 1303 },
    { CHANNEL_KEY: 102 },
    { CHANNEL_KEY: 1411 },
    { CHANNEL_KEY: 901 },
    { CHANNEL_KEY: 202 },
    { CHANNEL_KEY: 1 },
    { CHANNEL_KEY: 402 },
    { CHANNEL_KEY: 1100 },
    { CHANNEL_KEY: 404 },
    { CHANNEL_KEY: 1404 },
    { CHANNEL_KEY: 900 },
    { CHANNEL_KEY: 702 },
    { CHANNEL_KEY: 703 },
    { CHANNEL_KEY: 1400 },
    { CHANNEL_KEY: 1101 },
    { CHANNEL_KEY: 1304 },
    { CHANNEL_KEY: 103 },
  ]);
});
