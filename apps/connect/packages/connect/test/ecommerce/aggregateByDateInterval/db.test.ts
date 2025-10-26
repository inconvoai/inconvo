import { QuerySchema } from "~/types/querySchema";
import { groupBy } from "~/operations/groupBy";
import { getDb } from "~/dbConnection";

test("How many lineitems were sold each month?", async () => {
  const iql = {
    table: "fct_order_lineitem",
    whereAndArray: [],
    operation: "groupBy",
    operationParameters: {
      joins: null,
      groupBy: [
        {
          type: "dateInterval",
          column: "fct_order_lineitem.ORDER_TIMESTAMP",
          interval: "month",
          alias: "month_bucket",
        },
      ],
      count: ["fct_order_lineitem.ORDER_TIMESTAMP"],
      sum: null,
      min: null,
      max: null,
      avg: null,
      orderBy: { type: "groupKey", key: "month_bucket", direction: "asc" },
      limit: 12,
    },
  };

  const parsedQuery = QuerySchema.parse(iql);
  const db = await getDb();
  const response = await groupBy(db, parsedQuery);

  const byMonth = Object.fromEntries(
    response.data.map((row: any) => [
      row.month_bucket,
      { count: JSON.parse(row._count)["fct_order_lineitem.ORDER_TIMESTAMP"] },
    ])
  );

  expect(byMonth).toEqual({
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
