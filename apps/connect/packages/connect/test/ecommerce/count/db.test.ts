import { getPrismaClient } from "~/prismaClient";
import { QuerySchema } from "~/types/querySchema";
import { count } from "~/operations/count";

test("How many orders have we had?", async () => {
  const iql = {
    table: "fct_order",
    whereAndArray: [],
    operation: "count",
    operationParameters: {
      columns: ["unique_key"],
    },
  };

  const prisma = getPrismaClient();
  const parsedQuery = QuerySchema.parse(iql);
  const response = await count(prisma, parsedQuery);

  expect(response).toEqual({
    unique_key: 16144,
  });
});
