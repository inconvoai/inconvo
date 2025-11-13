import { operationDocs } from "./operationDocs";
import { whereConditionDocsSummary } from "./whereDocs";

const formatOperationExamples = () => {
  const operations = Object.entries(operationDocs)
    .filter(([key]) => key !== "NONE")
    .map(([operation, details]) => {
      const exampleEntries = Array.isArray(details.examples)
        ? details.examples
        : [];

      const examplesText = exampleEntries
        .map((ex, index) => {
          const label =
            exampleEntries.length > 1
              ? `Example ${index + 1}`
              : "Example";
          return `
${label}: ${ex.question}
Query: ${JSON.stringify(ex.query, null, 2)}`;
        })
        .join("");

      return `- **${operation}**: ${details.description}${examplesText}`;
    })
    .join("\n\n");

  return operations;
};

export const databaseRetrieverToolDescription = `Call this tool to retrieve data from the database. This tool provides access to various database operations for querying, aggregating, and analyzing data.
## Available Operations:
${formatOperationExamples()}

## Where Conditions (Filtering):
You can filter data using where conditions on any query. The where parameter accepts an array of conditions that can filter based on:
${whereConditionDocsSummary}

## Important Notes:
- Always specify the table name you want to query
- Choose the appropriate operation based on what data you need
- Use where conditions to filter results when needed
- For time-based grouping, use dateInterval keys for timelines ({ type: "dateInterval", column: "table.dateColumn", interval: "month" }) or dateComponent keys for recurring cycles ({ type: "dateComponent", column: "table.dateColumn", component: "dayOfWeek" })
- Provide aggregate lists (count, sum, min, max, avg) as arrays of fully-qualified column names, or null when not needed
- When joining tables, add a joins array where each hop pairs fully-qualified source/target columns (for example: "path": [{ "source": ["users.id"], "target": ["orders.user_id"] }])
- Column names in joined queries must use 'table.column' format
- OrderBy is optional for operations that support it - set to null if not needed
- Limit can be used to restrict the number of results returned
- Never ask the database retriever for computed columns - only ever mention columns that exist in the schema`;
