import type { Runnable } from "@langchain/core/runnables";
import {
  inconvoAgentPrompt,
  selectTablePrompt,
  selectOperationPrompt,
  whereConditionPrompt,
  extendQueryPrompt,
} from "../prompts";

// Map prompt base names (without version hash) to local prompts
const PROMPT_MAP: Record<string, Runnable> = {
  inconvo_agent_gpt5_dev: inconvoAgentPrompt,
  select_table: selectTablePrompt,
  select_operation: selectOperationPrompt,
  where_condition_agent_5: whereConditionPrompt,
  extend_query: extendQueryPrompt,
};

export async function getPrompt(promptName: string): Promise<Runnable> {
  // Extract base name (ignore version hash after colon)
  const baseName = promptName.split(":")[0];
  const prompt = PROMPT_MAP[baseName];

  if (!prompt) {
    throw new Error(
      `Prompt "${baseName}" not found. Available prompts: ${Object.keys(PROMPT_MAP).join(", ")}`,
    );
  }

  return prompt;
}
