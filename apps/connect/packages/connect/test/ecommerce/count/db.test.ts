import { QuerySchema } from "~/types/querySchema";
import { count } from "~/operations/count";
import { getDb } from "~/dbConnection";

test("How many orders have we had?", async () => {
  const iql = {
    table: "fct_order",
    whereAndArray: [],
    operation: "count",
    operationParameters: {
      columns: ["_unique_key"],
    },
  };

  const parsedQuery = QuerySchema.parse(iql);
  const db = await getDb();
  const response = await count(db, parsedQuery);

  expect(response).toEqual({
    _count: {
      _unique_key: 16144,
    },
  });
});
