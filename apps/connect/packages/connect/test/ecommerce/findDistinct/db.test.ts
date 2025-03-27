import { QuerySchema } from "~/types/querySchema";
import { findDistinct } from "~/operations/findDistinct";
import { getDb } from "~/dbConnection";

test("Find Unique Channel Keys for an order", async () => {
  const iql = {
    table: "fct_order",
    whereAndArray: [],
    operation: "findDistinct",
    operationParameters: {
      column: "CHANNEL_KEY",
    },
  };

  const parsedQuery = QuerySchema.parse(iql);
  const db = await getDb();
  const response = await findDistinct(db, parsedQuery);

  expect(response).toEqual([
    { CHANNEL_KEY: 103 },
    { CHANNEL_KEY: 1103 },
    { CHANNEL_KEY: 101 },
    { CHANNEL_KEY: 1000 },
    { CHANNEL_KEY: 302 },
    { CHANNEL_KEY: 1303 },
    { CHANNEL_KEY: 102 },
    { CHANNEL_KEY: 1902 },
    { CHANNEL_KEY: 1307 },
    { CHANNEL_KEY: 1411 },
    { CHANNEL_KEY: 901 },
    { CHANNEL_KEY: 202 },
    { CHANNEL_KEY: 1 },
    { CHANNEL_KEY: 402 },
    { CHANNEL_KEY: 1100 },
    { CHANNEL_KEY: 404 },
    { CHANNEL_KEY: 1311 },
    { CHANNEL_KEY: 1404 },
    { CHANNEL_KEY: 900 },
    { CHANNEL_KEY: 702 },
    { CHANNEL_KEY: 703 },
    { CHANNEL_KEY: 1400 },
    { CHANNEL_KEY: 1101 },
    { CHANNEL_KEY: 1304 },
  ]);
});
