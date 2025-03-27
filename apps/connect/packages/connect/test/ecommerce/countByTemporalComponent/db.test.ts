import { QuerySchema } from "~/types/querySchema";
import { countByTemporalComponent } from "~/operations/countByTemporalComponent";
import { getDb } from "~/dbConnection";

test("How many lineitems have been sold per day of the week?", async () => {
  const iql = {
    table: "fct_order_lineitem",
    whereAndArray: [],
    operation: "countByTemporalComponent",
    operationParameters: {
      component: "Day",
      dateColumn: "ORDER_TIMESTAMP",
    },
  };

  const parsedQuery = QuerySchema.parse(iql);
  const db = await getDb();
  const response = await countByTemporalComponent(db, parsedQuery);

  expect(response).toEqual({
    Wednesday: 1243,
    Thursday: 1222,
    Friday: 1318,
    Saturday: 1201,
    Sunday: 1080,
    Monday: 1145,
    Tuesday: 1194,
  });
});
