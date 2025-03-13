import { getPrismaClient } from "~/prismaClient";
import { QuerySchema } from "~/types/querySchema";
import { countRelations } from "~/operations/countRelations";

test("What was the order with the most line items?", async () => {
  const iql = {
    table: "fct_order",
    whereAndArray: [],
    operation: "countRelations",
    operationParameters: {
      columns: ["unique_key", "store_key"],
      relationsToCount: [
        {
          name: "fct_order_lineitem",
          distinct: null,
        },
      ],
      orderBy: {
        relation: "fct_order_lineitem",
        direction: "desc",
      },
      limit: 1,
    },
  };

  const prisma = getPrismaClient();
  const parsedQuery = QuerySchema.parse(iql);
  const response = await countRelations(prisma, parsedQuery);

  expect(response).toEqual([
    {
      unique_key: "5969239408944",
      store_key: "25824624728",
      _count: {
        fct_order_lineitem: 5,
      },
    },
  ]);
}, 10000);
