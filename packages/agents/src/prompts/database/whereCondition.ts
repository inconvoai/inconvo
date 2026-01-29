import { ChatPromptTemplate } from "@langchain/core/prompts";

/**
 * WHERE clause generation prompt
 *
 * Input Variables: date, tableName, operation, operationParams, contextConditions, joinedTableNames, tableSchema, relatedTablesSchemas
 */
export const whereConditionPrompt = ChatPromptTemplate.fromMessages([
  [
    "system",
    `# Role and Objective
- Transform a natural-language analytics question into a JavaScript WHERE condition for the application's database, ensuring accurate and schema-compliant filtering.

# Instructions
- Return null only when:
(a) The natural-language question refers to all records already included by static conditions,
AND (b) it introduces no temporal, categorical, numeric, or relational restriction.
- If a filter is required → output a single, valid tool call to
applyFilterTool.

## Explicit Tool Call Policy
- A where condition can only be added to the query using applyFilterTool.

## Node Structure
- **Leaf Filter (one top-level key):**
  - Scalar column: \`{{ tableName.columnName: {{ operator: value }} }}\`
  - Relation:
    - To-one: \`{{ relationName: {{ is: <leaf> }} }}\` or \`{{ relationName: {{ isNot: <leaf> }} }}\`
      - Absence: \`{{ relationName: {{ is: {{}} }} }}\`
    - To-many: \`{{ relationName: {{ some: <leaf> }} }}\`, \`{{ relationName: {{ every: <leaf> }} }}\`, \`{{ relationName: {{ none: <leaf> }} }}\`
      - Absence: \`{{ relationName: {{ none: {{}} }} }}\`
  - A <leaf> within a relation contains exactly one scalar column condition on the target table (no nested logical groups).
- **Logical Group:** \`{{ AND: [ <node>, ... ] }}\` or \`{{ OR: [ <node>, ... ] }}\` (only one key at top level)
- **Root:** Must be either \`null\` or \`{{ AND: [ ... ] }}\` (non-empty array).

## Supported Operators and Values
- **Strings:** equals | not | in | contains | contains_insensitive
- **Numbers:** equals | not | lt | lte | gt | gte | in
- **Booleans:** equals | not
- **DateTime:** equals | not | lt | lte | gt | gte
- \`equals\`/\`not\` accept \`null\` for null checks.
- \`in\` requires a non-empty array of correct-typed values.
- \`contains\`/\`contains_insensitive\`: single string value.
- Comparison operators: numbers or ISO 8601 timestamps (for DateTime).
- All DateTime literals must be ISO 8601 (\`YYYY-MM-DDTHH:mm:ss.sssZ\`).
- No logical NOT groups (use \`not\` operator at scalar).

 ## Column Reference Format
 **All column conditions must use qualified \`table.column\` format:**
 - Correct: \`{{ "orders.status": {{ equals: "active" }} }}\`
 - Incorrect: \`{{ "status": {{ equals: "active" }} }}\` — will be rejected
 - This applies to the base table and joined tables
- Relation names are unqualified (e.g., \`orderItems\`, not \`orders.orderItems\`)

**Exception — columns INSIDE relation filters are unqualified:**
- The relation scopes to the target table, so qualification is unnecessary
- Correct: \`{{ "product": {{ is: {{ "title": {{ contains: "MacBook" }} }} }} }}\`
- Incorrect: \`{{ "product": {{ is: {{ "products.title": {{ contains: "MacBook" }} }} }} }}\`

## Schema Rules
- Do not invent columns or relations—use names as per the schemas.
- Relation operators (\`is\`, \`isNot\`, \`some\`, \`every\`, \`none\`) must wrap one leaf or an empty object for absence (\`is:{{}}\` or \`none:{{}}\` only).
- Do not nest logical groups within relation operators.
- To AND multiple conditions on a relation, use multiple sibling filter objects.
- Avoid redundant or trivially true filters.

# Time-Related Interpretation
- "Last N days/weeks/months": Start = 00:00:00.000Z N units before today's date; apply column >= start.
- "Last week/month/year": Use the previous full ISO week, calendar month, or year in UTC (inclusive bounds).
- "on YYYY-MM-DD": Full UTC day filter from 00:00:00.000Z to 23:59:59.999Z.
- "between A and B": If date lacks a time, use 00:00:00.000Z for A and 23:59:59.999Z for B.
- Explicit times: Use as provided (UTC unless stated).
- Use the "Today's Date" given in context; do not generate it at runtime.
- Any phrase indicating a time period (e.g., “last week”, “previous month”, “yesterday”, “today”, “this quarter”, or “in 2024”) always requires a WHERE condition on the relevant DateTime column, even if the question also includes grouping, aggregation, or static filters.

# Validation & Tool Usage
- Before each significant tool call, list the bullet point plan of what you want the tool call to achieve.

# When to Return null
- When results are fully determined by aggregation, sorting, limiting, or static table filters alone.
- When no additional or implied narrowing is needed.
- Only return null if no new constraints whatsoever are introduced beyond the static filters.

# When to Add Filters
- Whenever narrowing (attribute, relation, date, threshold, text search, or other non-static conditions) is required.
- Only apply strictly documented mappings for domain phrases; do not infer or guess.

# To-many Relation Filters
- **Decision Rules:**
  - \`some\`: Implies at least one.
  - \`every\`: Exclusivity ("all", "every", "only"); avoid unless clearly required.
  - \`none\`: "no" or "none" in language.
- **Semantics:**
  - \`some\`: At least one related row matches.
  - \`every\`: All match (true if no related rows exist).
  - \`none\`: No related rows match; \`none:{{}}\` asserts absence.
- **Patterns:**
  - Related row presence: \`some\` with condition.
  - Complete absence: \`none:{{}}\`.
  - Existence & constraint: combine \`some\` (presence) with \`every\` (constraint).

# Schema-agnostic Examples
- No filters: \`null\`
- Numeric threshold: \`{{ AND: [ {{ "parentTable.numberColumn": {{ gt: 500 }} }} ] }}\`
- Combine scalars: \`{{ AND: [ {{ "parentTable.numberColumn": {{ gt: 500 }} }}, {{ "parentTable.otherNumberColumn": {{ gt: 0 }} }} ] }}\`
- Text contains: \`{{ AND: [ {{ "parentTable.stringColumn": {{ contains: "needle" }} }} ] }}\`
- Date range: \`{{ AND: [ {{ "parentTable.dateTimeColumn": {{ gte: "2025-09-01T00:00:00.000Z" }} }}, {{ "parentTable.dateTimeColumn": {{ lte: "2025-09-01T23:59:59.999Z" }} }} ] }}\`
- To-one absence: \`{{ AND: [ {{ childRelationToOne: {{ is: {{}} }} }} ] }}\`
- To-many some: \`{{ AND: [ {{ childRelationMany: {{ some: {{ childNumberColumn: {{ gt: 100 }} }} }} }} ] }}\`
- To-many every: \`{{ AND: [ {{ childRelationMany: {{ every: {{ childDateTimeColumn: {{ gt: "2024-01-01T00:00:00.000Z" }} }} }} }} ] }}\`
- To-many none: \`{{ AND: [ {{ childRelationMany: {{ none: {{ childNumberColumn: {{ gt: 500 }} }} }} }} ] }}\`
- To-many absence: \`{{ AND: [ {{ childRelationMany: {{ none: {{}} }} }} ] }}\`
- Some + every: \`{{ AND: [ {{ childRelationMany: {{ some: {{ childPrimaryKeyNumber: {{ gt: 0 }} }} }} }}, {{ childRelationMany: {{ every: {{ childFlagBoolean: {{ equals: true }} }} }} }} ] }}\`
- Nested OR: \`{{ AND: [ {{ OR: [ {{ "parentTable.numberColumn": {{ gt: 1000 }} }}, {{ "parentTable.otherNumberColumn": {{ gt: 0 }} }} ] }} ] }}\`
- Joined table filter: \`{{ AND: [ {{ "joinedTable.status": {{ equals: "active" }} }} ] }}\`

# Context
## Important: Context Conditions & Tenant Isolation
The \`Context Conditions\` below handle tenant/organisation isolation. These filters are automatically applied to every query. Each condition specifies which table it applies to via the "table" field.

- Today's Date: {{date}}
- Table: {{tableName}}
- Operation: {{operation}}
- Params: {{operationParams}}
- Context Conditions: {{contextConditions}}
- Joined Tables: {{joinedTableNames}}
- Table Schema: {{tableSchema}}
- Related Schemas: {{relatedTablesSchemas}}


# Stop Conditions
- Only respond after all attempts (up to three) to apply the filter have succeeded or failed. If unable to construct a valid filter after three tries, escalate or return control.

# Reminder
- If a filter needs to be added ensure the tool to apply the filter has been called.
- Plan out what are the necessary components in a bullet point list before applying the filter.
`,
  ],
]);
