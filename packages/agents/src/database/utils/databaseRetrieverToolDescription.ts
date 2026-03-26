import { operationDocs } from "./operationDocs";
import { whereConditionDocsSummary } from "./whereDocs";

function normalizeExampleQuery(query: Record<string, unknown>) {
  const { whereAndArray, questionConditions, ...rest } = query;

  return {
    ...rest,
    questionConditions:
      questionConditions ??
      (Array.isArray(whereAndArray) && whereAndArray.length > 0
        ? whereAndArray.at(-1)
        : null),
  };
}

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
Query: ${JSON.stringify(
            normalizeExampleQuery(ex.query as Record<string, unknown>),
            null,
            2,
          )}`;
        })
        .join("");

      return `- **${operation}**: ${details.description}${examplesText}`;
    })
    .join("\n\n");

  return operations;
};

export const databaseRetrieverToolDescription = `Call this tool to validate and execute a complete structured query draft against the selected database.

**IMPORTANT:** This tool does not plan for you. Before calling it, you must inspect the relevant schemas with \`getSchemasForTables\` and build a complete \`query\` object yourself.

## Required Query Shape
\`\`\`json
{
  "database": "databaseName",
  "query": {
    "table": "base_table_name",
    "operation": "findMany | findDistinct | count | countRelations | aggregate | aggregateGroups | groupBy",
    "operationParameters": { "...": "full operation-specific object" },
    "questionConditions": { "AND": [ ... ] } | null
  }
}
\`\`\`

## Available Operations:
${formatOperationExamples()}

## Question Conditions (Filtering):
Use \`questionConditions\` for user-requested filters. The filter DSL supports:
${whereConditionDocsSummary}

## Important Notes:
- Always specify the base table in \`query.table\`.
- Put the full operation-specific payload in \`query.operationParameters\`.
- Put only user-requested filters in \`query.questionConditions\`. Context-based row filters are enforced internally.
- **Dates must be absolute ISO strings:** Provide concrete ISO date strings (e.g., "2026-01-18"), not relative expressions. Calculate the actual date before calling this tool.
- In HAVING clauses, use \`{ type: "groupKey", key: <groupBy alias>, ... }\` for group-key filters (not \`{ type: "key" }\`); use \`{ type: "aggregate", function: <count|sum...>, column: <alias.column>, ... }\` for aggregate filters. For aggregateGroups, you can omit unused aggregate families entirely.
- Provide aggregate lists (count, sum, min, max, avg) as arrays of fully-qualified column names (\`alias.column\`), or null when not needed.
- When joining tables, add a \`joins\` array where each hop pairs fully-qualified source/target columns (e.g., \`"path": [{ "source": ["users.id"], "target": ["orders.user_id"] }]\`).
- Column references use \`alias.column\` format, where the alias is the join's \`name\` (or the table name if no alias is set). Multi-segment aliases are supported (e.g., \`orders.customer.total\` where \`orders.customer\` is the join alias and \`total\` is the column). If you set a custom alias, use it consistently in all references.
- Use exact schema column names from \`getSchemasForTables\`. Do not substitute semantically similar fields such as \`name\`, \`title\`, \`label\`, \`amount\`, or \`total\`.
- **questionConditions filtering:**
  - Base-table scalar filters use \`table.column\` keys.
  - Joined scalar filters use the selected join alias in \`alias.column\` keys.
  - Relation filters use the bare relation name as the key, not \`table.relation\`. Inside a relation filter, use unqualified column names from the related table.
  - When you already have a join defined, prefer scalar alias-qualified filters. Use relation-based filters (\`some\`/\`every\`/\`none\`) for existence checks or when no join is present.
- **findMany output format:** Results are returned with flattened column names: \`{table}_{column}\` for base table columns (e.g., \`users_id\`), and \`{alias}_{column}\` for joined columns where the alias keeps its dots (e.g., \`users.orders_subtotal\`).
- **Multiple joins to same table:** You can join the same table multiple times using different aliases (e.g., join \`users\` as both \`orders.buyer\` and \`orders.seller\`). Each alias creates a separate join with its own columns in the output.
- OrderBy is optional for operations that support it — set to null if not needed. Limit restricts the number of results returned.
- If the tool returns a validation error, repair the structured query draft using \`error.stage\`, \`error.message\`, and \`error.issues\`, then retry.
- Only reference columns that exist in the schema — do not invent ad-hoc computed expressions.`;
