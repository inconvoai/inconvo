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
            direction: "desc",
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
      response: { _count: { id: 41, name: 40 } },
    },
  },
  countWithJoin: {
    description:
      "Returns the count of non-null values after joining with related tables. You can count matching records using `count`, or count distinct values using `countDistinct`. Column names must be in 'table.column' format. Supports join types: 'inner' (default), 'left', 'right'. The joinPath follows the format 'parentTable.relationName' where relationName is the defined relationship in your schema.",
    example: {
      question:
        "Count the number of posts and users who has made posts by users with a verified email.",
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
          countDistinct: ["user.id"],
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
        _countDistinct: {
          "user.id": 8,
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
      "Returns aggregated values (min, max, average, sum, count, median) for specified columns in a table. Each aggregation type can be applied to different columns. Only supports columns of type number (or Date for MIN and Max only)",
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
      "Groups rows by one or more columns in starting table or a joined table. Can calculate the Count, Sum, Min, Max or Avg of the columns in the starting table or joined tabled grouped by the grouped columns. If a join is not needed set joins to null. The results are ordered in descending order and limited to a chosen value. If you want to groupBy a date column use groupByDateInterval instead. When grouping by an ID column, also add a friendly name to the groupBy array to present to the user later. Column names in groupBy array must be in 'table.column' format. Supports join types: 'inner' (default), 'left', 'right'.",
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
            function: "sum",
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
      "Calculates aggregations (min, max, count, sum, or avg) of records grouped by a given date interval. (day, week, month, year). This is useful for time series analysis. OrderBy can be 'chronological', 'reverseChronological', or an object with function, column, and direction. If the order is not specified you should use 'chronological' as the default order. 'reverseChronological' can be used to find the X most recent records grouped by the specified date interval.",
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
        { date_interval: "2021-02-01", _avg: { totalAmount: 145.75 } },
        { date_interval: "2021-03-01", _avg: { totalAmount: 132.25 } },
        { date_interval: "2021-01-01", _avg: { totalAmount: 120.5 } },
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
          component: "Day",
          dateColumn: "createdAt",
        },
      },
      response: {
        Monday: 1,
        Tuesday: 4,
        Wednesday: 10,
        Thursday: 2,
        Friday: 3,
        Saturday: 1,
        Sunday: 0,
      },
    },
  },
  NONE: {
    description: "No operation selected.",
  },
};
