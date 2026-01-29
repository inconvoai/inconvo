import type { Runnable } from "@langchain/core/runnables";
import {
  inconvoAgentPrompt,
  selectTablePrompt,
  selectOperationPrompt,
  whereConditionPrompt,
  extendQueryPrompt,
} from "../prompts";

// Map prompt names to local prompts
const PROMPT_MAP: Record<string, Runnable> = {
  inconvo_agent: inconvoAgentPrompt,
  select_table: selectTablePrompt,
  select_operation: selectOperationPrompt,
  where_condition_agent_5: whereConditionPrompt,
  extend_query: extendQueryPrompt,
};

export async function getPrompt(promptName: string): Promise<Runnable> {
  const prompt = PROMPT_MAP[promptName];

  if (!prompt) {
    throw new Error(
      `Prompt "${promptName}" not found. Available prompts: ${Object.keys(PROMPT_MAP).join(", ")}`,
    );
  }

  return prompt;
}
