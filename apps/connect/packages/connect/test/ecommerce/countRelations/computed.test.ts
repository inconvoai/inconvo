import { getPrismaClient } from "~/prismaClient";
import { QuerySchema } from "~/types/querySchema";
import { countRelations } from "~/operations/countRelations";

test("What was the order with the most lineitems with an order profit of over $100", async () => {
  const iql = {
    table: "fct_order",
    computedColumns: [
      {
        name: "profit",
        ast: {
          mathjs: "OperatorNode",
          op: "+",
          fn: "add",
          args: [
            {
              mathjs: "OperatorNode",
              op: "+",
              fn: "add",
              args: [
                {
                  mathjs: "SymbolNode",
                  name: "ORDER_PRODUCT_GROSS_REVENUE",
                },
                {
                  mathjs: "SymbolNode",
                  name: "ORDER_PRODUCT_TAX",
                },
              ],
            },
            {
              mathjs: "SymbolNode",
              name: "ORDER_PRODUCT_COGS",
            },
          ],
        },
        type: "Decimal",
      },
    ],
    whereAndArray: [
      {
        profit: {
          gt: 100,
        },
      },
    ],
    operation: "countRelations",
    operationParameters: {
      columns: ["unique_key", "store_key"],
      relations: ["fct_order_lineitem"],
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

  const answer = {
    unique_key: "5510048383280",
    store_key: "25824624728",
    _count: {
      fct_order_lineitem: 3,
    },
    profit: 198,
  };

  const keysToCompare = Object.keys(answer);
  const filteredProxyObject = Object.fromEntries(
    Object.entries(response[0]).filter(([key]) => keysToCompare.includes(key))
  );

  expect(filteredProxyObject).toEqual(answer);
});
