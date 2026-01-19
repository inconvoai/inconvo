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
            exampleEntries.length > 1 ? `Example ${index + 1}` : "Example";
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

**IMPORTANT:** This tool operates in complete isolation with no access to conversation history or previous results. Your question must be fully self-contained with all necessary values explicitly included.

## Available Operations:
${formatOperationExamples()}

## Where Conditions (Filtering):
You can filter data using where conditions on any query. The where parameter accepts an array of conditions that can filter based on:
${whereConditionDocsSummary}

## Important Notes:
- Always specify the table name you want to query
- Choose the appropriate operation based on what data you need
- Use where conditions to filter results when needed
- **Dates must be absolute ISO strings:** When filtering or grouping by dates, always provide concrete ISO date strings (e.g., "2026-01-18") rather than relative expressions like "now()-30d" or "last 30 days". Calculate the actual date before calling this tool.
- **Prefer aggregation over findMany for counts and summaries:**
  - When you need counts, totals, averages, or distributions, use \`groupBy\`, \`count\`, or \`aggregate\` operations
  - Do NOT use \`findMany\` to fetch all records and count them later — this is inefficient and may hit row limits
  - For time-based distributions, use \`groupBy\` with dateInterval or dateComponent keys
  - dateComponent options: hour, dayOfWeek, dayOfMonth, month, year (for recurring patterns)
  - dateInterval options: hour, day, week, month, quarter, year (for timeline grouping)
- For time-based grouping, use dateInterval keys for timelines ({ type: "dateInterval", column: "table.dateColumn", interval: "month" }) or dateComponent keys for recurring cycles ({ type: "dateComponent", column: "table.dateColumn", component: "dayOfWeek" })
- In HAVING clauses, use { type: "groupKey", key: <groupBy alias>, ... } for group-key filters (not { type: "key" }); use { type: "aggregate", function: <count|sum...>, column: <table.column>, ... } for aggregate filters. For aggregateGroups, you can omit unused aggregate families entirely (no need to send nulls/empties).
- Provide aggregate lists (count, sum, min, max, avg) as arrays of fully-qualified column names, or null when not needed
- When joining tables, add a joins array where each hop pairs fully-qualified source/target columns (for example: "path": [{ "source": ["users.id"], "target": ["orders.user_id"] }])
- Column names in joined queries must use 'table.column' format
- **Filtering on joined tables:** When you have joins, you can filter directly on joined table columns using table.column format in where conditions (e.g., { "orders.status": { equals: "active" } }). This is simpler than using relation-based subquery filters when you already have a join defined.
- **findMany output format:** Results are returned with flattened column names: \`{table}_{column}\` for base table columns (e.g., \`users_id\`), and \`{alias}_{column}\` for joined columns with dots replaced by double underscores (e.g., \`users__orders_subtotal\`).
- **Multiple joins to same table:** You can join the same table multiple times using different aliases (e.g., join \`users\` as both \`orders.buyer\` and \`orders.seller\`). Each alias creates a separate join with its own columns in the output.
- OrderBy is optional for operations that support it - set to null if not needed
- Limit can be used to restrict the number of results returned
- Never ask the database retriever for computed columns - only ever mention columns that exist in the schema`;
