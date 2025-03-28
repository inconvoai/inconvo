import { QuerySchema } from "~/types/querySchema";
import { countRelations } from "~/operations/countRelations";
import { getDb } from "~/dbConnection";

test("What was the order with the most line items?", async () => {
  const iql = {
    table: "fct_order",
    whereAndArray: [],
    operation: "countRelations",
    operationParameters: {
      columns: ["_unique_key", "store_key"],
      relationsToCount: [
        {
          name: "fct_order_lineitems",
          distinct: null,
        },
      ],
      orderBy: {
        relation: "fct_order_lineitems",
        direction: "desc",
      },
      limit: 1,
    },
  };

  const parsedQuery = QuerySchema.parse(iql);
  const db = await getDb();
  const response = await countRelations(db, parsedQuery);

  expect(response).toEqual([
    {
      _unique_key: "5969239408944",
      store_key: "25824624728",
      _count: {
        fct_order_lineitems: 5,
      },
    },
  ]);
}, 10000);
