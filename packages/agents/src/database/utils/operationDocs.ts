export const operationDocs = {
  findMany: {
    description:
      "Returns one or more records for a given table using a `select` map keyed by table alias. Each key lists the columns to fetch for that alias. You can optionally include joins and an order clause (set to null if not ordering). Output columns are flattened with format: `table_column` for base table, `alias_column` for joined tables (dots in alias become underscores).",
    examples: [
      {
        question:
          "Find the name, and address of the 2 most recently created users.",
        query: {
          table: "user",
          operation: "findMany",
          operationParameters: {
            select: {
              user: ["name"],
              "user.address": ["line1", "line2", "city", "state", "zip"],
            },
            joins: [
              {
                table: "address",
                name: "user.address",
                path: [
                  {
                    source: ["user.address_id"],
                    target: ["address.id"],
                  },
                ],
              },
            ],
            orderBy: {
              column: "createdAt",
              direction: "desc",
            },
            limit: 2,
          },
        },
        response: [
          {
            user_name: "John",
            "user__address_line1": "123 Main St",
            "user__address_line2": "Apt 2",
            "user__address_city": "Boston",
            "user__address_state": "MA",
            "user__address_zip": "02110",
          },
          {
            user_name: "Joe",
            "user__address_line1": "456 Elm St",
            "user__address_line2": "Apt 3",
            "user__address_city": "New York",
            "user__address_state": "NY",
            "user__address_zip": "10001",
          },
        ],
      },
      {
        question:
          "For the 5 most recently created users, show their orders with subtotal and product name.",
        query: {
          table: "users",
          operation: "findMany",
          operationParameters: {
            select: {
              users: ["id", "name", "created_at"],
              "users.orders": ["id", "subtotal", "created_at"],
              "users.orders.products": ["title"],
            },
            joins: [
              {
                table: "orders",
                name: "users.orders",
                path: [
                  {
                    source: ["users.id"],
                    target: ["orders.user_id"],
                  },
                ],
              },
              {
                table: "products",
                name: "users.orders.products",
                path: [
                  {
                    source: ["users.id"],
                    target: ["orders.user_id"],
                  },
                  {
                    source: ["orders.product_id"],
                    target: ["products.id"],
                  },
                ],
              },
            ],
            orderBy: {
              column: "created_at",
              direction: "desc",
            },
            limit: 5,
          },
        },
        response: [
          {
            users_id: 1,
            users_name: "Alice",
            users_created_at: "2024-10-01T12:00:00.000Z",
            "users__orders_id": 301,
            "users__orders_subtotal": 199.99,
            "users__orders_created_at": "2024-10-02T09:00:00.000Z",
            "users__orders__products_title": "Noise Cancelling Headphones",
          },
        ],
      },
    ],
  },
  findDistinct: {
    description:
      "Returns a list of distinct values for a given column in a table. Columns must be fully qualified (`table.column`).",
    examples: [
      {
        question: "Find all distinct countries in the user table.",
        query: {
          table: "user",
          operation: "findDistinct",
          operationParameters: {
            column: "user.country",
          },
        },
        response: [{ country: "USA" }, { country: "UK" }, { country: "IRE" }],
      },
    ],
  },
  count: {
    description:
      "Returns the count and/or count distinct of specified columns in a table. Use `count` for regular counts (including `_all` for count(*)); remember that counting a column aggregates every row produced by the joins, so choose `countDistinct` whenever you only want unique values. Base-table metrics must be written as `table.column`; joined metrics must be `alias.column`. Provide at least one of `count` or `countDistinct` and set the unused option to null. Include `joins` when a metric references a related table; each hop pairs fully-qualified source/target columns in the relationship path.",
    examples: [
      {
        question:
          "Count the number of orders and the number of distinct customers who placed orders.",
        query: {
          table: "order",
          operation: "count",
          operationParameters: {
            count: ["order.id", "_all"],
            countDistinct: ["order.customerId"],
          },
        },
        response: {
          _count: { "order.id": 100, _all: 100 },
          _countDistinct: { "order.customerId": 45 },
        },
      },
      {
        question:
          "How many posts did verified users create, and how many unique users posted?",
        query: {
          table: "user",
          operation: "count",
          operationParameters: {
            joins: [
              {
                table: "posts",
                name: "posts",
                path: [
                  {
                    source: ["user.id"],
                    target: ["posts.user_id"],
                  },
                ],
                joinType: "inner",
              },
            ],
            count: ["posts.id"],
            countDistinct: ["user.id"],
          },
          whereAndArray: [
            {
              emailVerified: { equals: true },
            },
          ],
        },
        response: {
          _count: {
            "posts.id": 10,
          },
          _countDistinct: {
            "user.id": 8,
          },
        },
      },
    ],
  },
  countRelations: {
    description:
      "Returns the count of relations for each row in the starting table. Provide a `joins` array where each descriptor names the relation and supplies hop-based paths (source/target column pairs) so the connector can traverse the relationship. Result set matches the number of base rows. Optionally count distinct values per relation and choose an `orderBy` entry keyed by relation name. Set unused options to null.",
    examples: [
      {
        question: "How many posts does each user have?",
        query: {
          table: "user",
          operation: "countRelations",
          operationParameters: {
            columns: ["id", "name"],
            joins: [
              {
                table: "posts",
                name: "posts",
                path: [
                  {
                    source: ["user.id"],
                    target: ["posts.user_id"],
                  },
                ],
              },
            ],
            relationsToCount: [
              { name: "posts", distinct: null }, // distinct is optional/nullable
            ],
            orderBy: {
              name: "posts",
              direction: "desc",
            },
            limit: 2,
          },
        },
        response: [
          { id: 1, name: "John", _count: { posts: 4 } },
          { id: 2, name: "Joe", _count: { posts: 1 } },
        ],
      },
    ],
  },
  aggregate: {
    description:
      "Returns aggregated values (min, max, average, sum, count, countDistinct, median) for specified columns in a table. Provide fully-qualified column names (`alias.column`). When aggregating across relations, include hop-based join descriptors in `joins`. Numeric aggregates require numeric columns (computed columns count as numeric). MIN/MAX also support temporal columns. countDistinct counts unique values. Set unused aggregate arrays to null.",
    examples: [
      {
        question:
          "What is the minimum and maximum age, and average salary of users.",
        query: {
          table: "user",
          operation: "aggregate",
          operationParameters: {
            min: ["user.age", "user.salary"],
            max: ["user.age"],
            avg: ["user.salary"],
            sum: null,
            count: ["user.id"],
            countDistinct: null,
            median: null,
          },
        },
        response: {
          _min: { age: 20, salary: 50000 },
          _max: { age: 55 },
          _avg: { salary: 75000 },
          _count: { id: 100 },
        },
      },
    ],
  },
  groupBy: {
    description:
      'Groups rows by one or more keys drawn from the starting table or hop-based joins. Keys can be direct columns (`{ type: "column", column: "table.column" }`), date intervals (`{ type: "dateInterval", column: "table.dateColumn", interval: "month" }`), or recurring date components (`{ type: "dateComponent", column: "table.dateColumn", component: "dayOfWeek" }`). Aggregates (count, countDistinct, sum, min, max, avg) should be arrays of fully-qualified column names, or null when unused. countDistinct counts unique values per group. Provide joins as an array of descriptors with `table`, optional alias `name`, and `path` hops (each hop pairs source columns with target columns). Order the results either by an aggregate (type `"aggregate"`) or by one of the group keys (type `"groupKey"`). Optionally add `having` as an AND-only array of predicates on group keys or aggregates (operators: equals, not, in, notIn, lt, lte, gt, gte; null handling mirrors WHERE). Ensure every `groupBy` alias is unique. Supported join types: `inner` (default), `left`, `right`.',
    examples: [
      {
        question:
          "How many cars are owned by users in each country, sorted by the total?",
        query: {
          table: "user",
          operation: "groupBy",
          operationParameters: {
            joins: [
              {
                table: "country",
                name: "user.country",
                path: [
                  {
                    source: ["user.countryId"],
                    target: ["country.id"],
                  },
                ],
                joinType: "inner",
              },
            ],
            groupBy: [
              {
                type: "column",
                column: "country.name",
                alias: "country",
              },
            ],
            sum: ["user.cars"],
            min: null,
            max: null,
            count: null,
            countDistinct: null,
            avg: null,
            orderBy: {
              type: "aggregate",
              function: "sum",
              column: "user.cars",
              direction: "desc",
            },
            having: [
              {
                type: "aggregate",
                function: "sum",
                column: "user.cars",
                operator: "gt",
                value: 5,
              },
            ],
            limit: 3,
          },
        },
        response: [
          { country: "USA", _sum: { "user.cars": 12 } },
          { country: "UK", _sum: { "user.cars": 8 } },
          { country: "IRE", _sum: { "user.cars": 7 } },
        ],
      },
      {
        question: "How many orders were created each month during 2024?",
        query: {
          table: "orders",
          operation: "groupBy",
          operationParameters: {
            joins: null,
            groupBy: [
              {
                type: "dateInterval",
                column: "orders.createdAt",
                interval: "month",
                alias: "month",
              },
            ],
            sum: null,
            min: null,
            max: null,
            count: ["orders.id"],
            countDistinct: null,
            avg: null,
            orderBy: {
              type: "groupKey",
              key: "month",
              direction: "asc",
            },
            limit: 12,
          },
        },
        response: [
          { month: "2024-01", _count: { "orders.id": 120 } },
          { month: "2024-02", _count: { "orders.id": 135 } },
          { month: "2024-03", _count: { "orders.id": 142 } },
        ],
      },
      {
        question:
          "How many orders were created each day of the week over the last month?",
        query: {
          table: "orders",
          operation: "groupBy",
          operationParameters: {
            groupBy: [
              {
                type: "dateComponent",
                column: "orders.createdAt",
                component: "dayOfWeek",
                alias: "dayOfWeek",
              },
            ],
            joins: null,
            sum: null,
            min: null,
            max: null,
            count: ["orders.id"],
            countDistinct: null,
            avg: null,
            orderBy: {
              type: "groupKey",
              key: "dayOfWeek",
              direction: "asc",
            },
            limit: 7,
          },
        },
        response: [
          { dayOfWeek: "Monday", _count: { "orders.id": 42 } },
          { dayOfWeek: "Tuesday", _count: { "orders.id": 50 } },
          { dayOfWeek: "Wednesday", _count: { "orders.id": 38 } },
          { dayOfWeek: "Thursday", _count: { "orders.id": 45 } },
          { dayOfWeek: "Friday", _count: { "orders.id": 61 } },
          { dayOfWeek: "Saturday", _count: { "orders.id": 34 } },
          { dayOfWeek: "Sunday", _count: { "orders.id": 27 } },
        ],
      },
    ],
  },
  aggregateGroups: {
    description:
      'Runs a grouped query but returns a single summary row with optional reducers across groups. Use when you need counts or aggregate totals over groups without returning each group row. Provide `groupBy` keys (columns/date buckets), optional HAVING filters, and aggregates. HAVING on group keys must use `type: "groupKey"` and `key` matching a groupBy alias (not `type: "key"`). Aggregate HAVING entries must use `type: "aggregate"` with `function` + `column`. `groupCount` counts how many groups pass HAVING. Aggregates accept arrays of fully-qualified columns (count, countDistinct, sum, min, max, avg) and unused aggregates can be omitted entirely (no need to send null/empty arrays). Reducers fold per-group metrics across all groups; defaults: sum for count/countDistinct/sum, none for min/max/avg. No orderBy/limit.',
    examples: [
      {
        question: "How many users placed more than two orders?",
        query: {
          table: "orders",
          operation: "aggregateGroups",
          operationParameters: {
            joins: null,
            groupBy: [{ type: "column", column: "orders.user_id" }],
            having: [
              {
                type: "aggregate",
                function: "count",
                column: "orders.id",
                operator: "gt",
                value: 2,
              },
            ],
            aggregates: { groupCount: true },
          },
        },
        response: {
          groupCount: 3,
        },
      },
      {
        question:
          "Total orders from users with >2 orders and max orders by any one user in that cohort",
        query: {
          table: "orders",
          operation: "aggregateGroups",
          operationParameters: {
            groupBy: [{ type: "column", column: "orders.user_id" }],
            having: [
              {
                type: "aggregate",
                function: "count",
                column: "orders.id",
                operator: "gt",
                value: 2,
              },
            ],
            aggregates: { groupCount: true, count: ["orders.id"] },
            reducers: { count: ["sum", "max"] },
          },
        },
        response: {
          groupCount: 3,
          _count: { "orders.id": { sum: 12, max: 6 } },
        },
      },
    ],
  },
  NONE: {
    description: "No operation selected.",
    examples: [],
  },
};
