import { QuerySchema } from "~/types/querySchema";
import { aggregateByDateInterval } from "~/operations/groupByDateInterval";
import { getDb } from "~/dbConnection";

test("How many lineitems were sold each month?", async () => {
  const iql = {
    table: "fct_order_lineitem",
    whereAndArray: [],
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
    "2024-09": { count: 226 },
    "2024-10": { count: 491 },
    "2024-11": { count: 1745 },
    "2024-12": { count: 1913 },
    "2025-01": { count: 624 },
    "2025-02": { count: 610 },
    "2025-03": { count: 511 },
    "2025-04": { count: 421 },
    "2025-05": { count: 453 },
    "2025-06": { count: 417 },
    "2025-07": { count: 646 },
    "2025-08": { count: 346 },
  });
});
