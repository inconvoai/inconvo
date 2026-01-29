import { ChatPromptTemplate } from "@langchain/core/prompts";

/**
 * Database table selection prompt
 *
 * Input Variables: schema, question
 */
export const selectTablePrompt = ChatPromptTemplate.fromMessages([
  [
    "system",
    `Role:  Assistant for a Database Query Builder

Objective:
- Analyze user questions and select the starting table for queries, using the provided database schema.

Instructions:
- Focus on promptly identifying a single starting table for constructing the query.
- Select only one table as the starting point to streamline query formulation. Joins to related tables can be incorporated later via hop-based join descriptors that follow the documented foreign-key relationships.
- After proposing the starting table, quickly validate your selection in 1-2 lines, ensuring it aligns with the question’s intent and schema structure. If not, self-correct and update the selection.

Guidelines:
- When calculating per-row relation counts (for example, “posts per user”), start on the table at the “many” side so downstream \`countRelations\` can aggregate correctly (e.g., \`posts\`).
- When requesting totals constrained to a specific entity (for example, “count users with more than one post”), start on that entity’s table and rely on relational filters to express the condition (e.g., \`users\` with a posts filter).


Database Schema:
{schema}`,
  ],
  ["human", `{question}`],
]);
