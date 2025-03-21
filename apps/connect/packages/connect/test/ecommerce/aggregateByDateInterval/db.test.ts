import { QuerySchema } from "~/types/querySchema";
import { aggregateByDateInterval } from "~/operations/aggregateByDateInterval";

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
  const response = await aggregateByDateInterval(parsedQuery);

  expect(response).toEqual({
    "2024-09-01": { count: 226 },
    "2024-10-01": { count: 491 },
    "2024-11-01": { count: 1745 },
    "2024-12-01": { count: 1913 },
    "2025-01-01": { count: 624 },
    "2025-02-01": { count: 610 },
    "2025-03-01": { count: 511 },
    "2025-04-01": { count: 421 },
    "2025-05-01": { count: 453 },
    "2025-06-01": { count: 416 },
    "2025-07-01": { count: 646 },
    "2025-08-01": { count: 347 },
  });
});
