import { operationDocs } from "./operationDocs";
import { whereConditionDocsSummary } from "./whereDocs";

const formatOperationExamples = () => {
  const operations = Object.entries(operationDocs)
    .filter(([key]) => key !== "NONE")
    .map(([operation, details]) => {
      const example =
        "example" in details && details.example
          ? `
Example: ${details.example.question}
Query: ${JSON.stringify(details.example.query, null, 2)}`
          : "";

      return `- **${operation}**: ${details.description}${example}`;
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
- For groupBy operations on date columns, use groupByDateInterval instead
- When joining tables, ensure you specify the correct joinPath following the schema relationships
- Column names in joined queries must use 'table.column' format
- OrderBy is optional for operations that support it - set to null if not needed
- Limit can be used to restrict the number of results returned
- Never ask the database retriever for computed columns - only ever mention columns that exist in the schema`;
