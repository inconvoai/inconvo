import { getPrismaClient } from "~/prismaClient";
import { QuerySchema } from "~/types/querySchema";
import { countByDateInterval } from "~/operations/countByDateInterval";

test("How many lineitems were sold each month?", async () => {
  const iql = {
    table: "fct_order_lineitem",
    whereAndArray: [],
    operation: "countByDateInterval",
    operationParameters: {
      interval: "month",
      dateColumn: "ORDER_TIMESTAMP",
    },
  };

  const prisma = getPrismaClient();
  const parsedQuery = QuerySchema.parse(iql);
  const response = await countByDateInterval(prisma, parsedQuery);

  expect(response).toEqual({
    "2024-09-01": 226,
    "2024-10-01": 491,
    "2024-11-01": 1745,
    "2024-12-01": 1913,
    "2025-01-01": 624,
    "2025-02-01": 610,
    "2025-03-01": 511,
    "2025-04-01": 421,
    "2025-05-01": 453,
    "2025-06-01": 416,
    "2025-07-01": 646,
    "2025-08-01": 347,
  });
});
