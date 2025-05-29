export const operationDocs = {
  findMany: {
    description:
      "Returns 1 or more records for a given table and specified columns. You can optionally order the results with the order object, if not ordering set order to null. You may join to any related tables",
    example: {
      question:
        "Find the name, and address of the 2 most recently created users.",
      query: {
        table: "user",
        operation: "findMany",
        operationParameters: {
          columns: {
            user: ["name"],
            address: ["line1", "line2", "city", "state", "zip"],
          },
          orderBy: {
            column: "createdAt",
            direction: "asc",
          },
          limit: 2,
        },
      },
      response: [
        {
          name: "John",
          address: {
            line1: "123 Main St",
            line2: "Apt 2",
            city: "Boston",
            state: "MA",
            zip: "02110",
          },
        },
        {
          name: "Joe",
          address: {
            line1: "456 Elm St",
            line2: "Apt 3",
            city: "New York",
            state: "NY",
            zip: "10001",
          },
        },
      ],
    },
  },
  findDistinct: {
    description:
      "Returns a list of distinct values for a given column in a table.",
    example: {
      question: "Find all distinct countries in the user table.",
      query: {
        table: "user",
        operation: "findDistinct",
        operationParameters: {
          column: "country",
        },
      },
      response: [{ country: "USA" }, { country: "UK" }, { country: "IRE" }],
    },
  },
  count: {
    description:
      "Returns the number of non-null records for the specified columns in a table. You can also count all records by using the _all option which will run a count(*) on the table.",
    example: {
      question:
        "Count the number of non-null records for each column in the user table.",
      query: {
        table: "user",
        operation: "count",
        operationParameters: {
          columns: ["id", "name"],
        },
      },
      response: { id: 41, name: 40 },
    },
  },
  countWithJoin: {
    description:
      "Returns the count of non-null values after joining with related tables. It differs from countRelations by operating on a joined dataset instead of counting relations per row.",
    example: {
      question: "Count the number of posts by users with a verified email.",
      query: {
        table: "user",
        operation: "countWithJoin",
        operationParameters: {
          joins: [
            {
              table: "posts",
              joinPath: "user.posts",
              joinType: "inner",
            },
          ],
          count: ["posts.id"],
        },
        where: [
          {
            emailVerified: true,
          },
        ],
      },
      response: {
        _count: {
          "posts.id": 10,
        },
      },
    },
  },
  countRelations: {
    description:
      "Returns the count of relations for each row in the starting table. Result set will be of the same length as the number of rows in the starting table. You can optionally count distinct values for a specific column in each relation. You can also optionally order the results with the order object, if not ordering set order to null.",
    example: {
      question: "How many posts does each user have?",
      query: {
        table: "user",
        operation: "countRelations",
        operationParameters: {
          columns: ["id", "name"],
          relationsToCount: [
            { name: "posts", distinct: null }, // distinct is optional/nullable
          ],
          orderBy: {
            relation: "posts",
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
  },
  aggregate: {
    description:
      "Returns aggregated values (min, max, average, sum, count, median) for specified columns in a table. Each aggregation type can be applied to different columns.",
    example: {
      question:
        "What is the minimum and maximum age, and average salary of users.",
      query: {
        table: "user",
        operation: "aggregate",
        operationParameters: {
          min: ["age", "salary"],
          max: ["age"],
          avg: ["salary"],
          sum: null,
          count: ["id"],
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
  },
  groupBy: {
    description:
      "Groups rows by one or more columns in starting table or a joined table. Can calculate the Count, Sum, Min, Max or Avg of the columns in the starting table or joined tabled grouped by the grouped columns. If a join is not needed set join to null. The results are ordered in descending order and limited to a chosen value. If you want to groupBy a date column use groupByDateInterval instead. When grouping by an ID column, also add a friendly name to the groupBy array to present to the user later.",
    example: {
      question: "How many cars are owned by users in each country?",
      query: {
        table: "user",
        operation: "groupBy",
        operationParameters: {
          joins: [
            {
              table: "country",
              joinPath: "user.country",
              joinType: "inner",
            },
          ],
          groupBy: ["country.name"],
          sum: {
            columns: ["user.cars"],
          },
          min: null,
          max: null,
          count: null,
          orderBy: {
            function: "count",
            column: "user.cars",
            direction: "desc",
          },
          limit: 3,
        },
      },
      response: [
        { country: "USA", _sum: { cars: 12 } },
        { country: "UK", _sum: { cars: 8 } },
        { country: "IRE", _sum: { cars: 7 } },
      ],
    },
  },
  groupByDateInterval: {
    description:
      "Calculates aggregations (min, max, count, sum, or avg) of records grouped by a given date interval. (day, week, month, year). This is useful for time series analysis",
    example: {
      question: "Which months had the highest average order amounts?",
      query: {
        table: "orders",
        operation: "groupByDateInterval",
        operationParameters: {
          dateColumn: "orderDate",
          interval: "month",
          sum: null,
          min: null,
          max: null,
          count: null,
          avg: ["totalAmount"],
          orderBy: {
            function: "avg",
            column: "totalAmount",
            direction: "desc",
          },
          limit: 3,
        },
      },
      response: [
        { "2021-02-01": { _avg: { totalAmount: 145.75 } } },
        { "2021-03-01": { _avg: { totalAmount: 132.25 } } },
        { "2021-01-01": { _avg: { totalAmount: 120.5 } } },
      ],
    },
  },
  countByTemporalComponent: {
    description:
      "Calculates the count of records grouped by a specified temporal component. (day of the week, month of the year)",
    example: {
      question: "What is the number of new users created by day of the week?",
      query: {
        table: "user",
        operation: "countByTemporalComponent",
        operationParameters: {
          temporalComponent: "Day",
          dateColumn: "createdAt",
        },
      },
      response: [
        { temporalComponent: "Monday", count: 1 },
        { temporalComponent: "Tuesday", count: 4 },
        { temporalComponent: "Wednesday", count: 10 },
        { temporalComponent: "Thursday", count: 2 },
        { temporalComponent: "Friday", count: 3 },
        { temporalComponent: "Saturday", count: 1 },
        { temporalComponent: "Sunday", count: 0 },
      ],
    },
  },
  NONE: {
    description: "No operation selected.",
  },
};
