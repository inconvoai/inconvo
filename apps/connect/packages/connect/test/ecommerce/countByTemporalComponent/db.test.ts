import { QuerySchema } from "~/types/querySchema";
import { countByTemporalComponent } from "~/operations/countByTemporalComponent";

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
  const response = await countByTemporalComponent(parsedQuery);

  expect(response).toEqual({
    Wednesday: 1242,
    Thursday: 1218,
    Friday: 1325,
    Saturday: 1195,
    Sunday: 1085,
    Monday: 1144,
    Tuesday: 1194,
  });
});
