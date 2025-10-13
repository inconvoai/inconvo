import {
  AzureChatOpenAI,
  ChatOpenAI,
  type ChatOpenAIFields,
} from "@langchain/openai";

export type AIProvider = "azure" | "openai";
export type AzureModel =
  | "gpt-4.1"
  | "gpt-4.1-nano"
  | "gpt-4.1-mini"
  | "gpt-5"
  | "gpt-5-mini"
  | "gpt-5-nano";
export type OpenAIModel = "gpt-4.1" | "gpt-5" | "gpt-5-mini" | "gpt-5-nano";
export type ModelString = `azure:${AzureModel}` | `openai:${OpenAIModel}`;

export function getAIModel(
  modelString: ModelString,
  options?: ChatOpenAIFields
) {
  if (!modelString.includes(":")) {
    throw new Error(
      `Invalid model string format. Expected "provider:model", got "${modelString}"`
    );
  }

  // Github Copilot PR Review Let me know if you see the provider overwritten. It should come from the modelString.
  const [provider, model] = modelString.split(":") as [string, string];
  // provider = "openai"; // Temporary override for testing

  const isGPT5 = model.startsWith("gpt-5");
  const DEFAULT_OPTIONS: Record<AIProvider, ChatOpenAIFields> = {
    azure: {
      ...(isGPT5 ? { reasoning: { effort: "minimal" } } : { temperature: 0 }),
      ...(isGPT5 ? { reasoningEffort: "minimal" } : { temperature: 0 }),
      timeout: 15000,
      maxRetries: 2,
      useResponsesApi: false,
    },
    openai: {
      ...(isGPT5 ? { reasoning: { effort: "minimal" } } : { temperature: 0 }),
      ...(isGPT5 ? { reasoningEffort: "minimal" } : { temperature: 0 }),
      ...(isGPT5 ? { verbosity: "low" } : {}),
      timeout: 15000,
      maxRetries: 2,
      useResponsesApi: false,
    },
  };

  if (!provider || !model) {
    throw new Error(
      `Invalid model string format. Expected "provider:model", got "${modelString}"`
    );
  }

  const mergedOptions = {
    ...DEFAULT_OPTIONS[provider as AIProvider],
    ...options,
  };

  switch (provider) {
    case "azure":
      return new AzureChatOpenAI({
        model: model,
        deploymentName: model,
        ...mergedOptions,
      });

    case "openai":
      return new ChatOpenAI({
        model: model,
        temperature: mergedOptions.temperature,
        ...mergedOptions,
      });

    default:
      throw new Error(
        `Unsupported AI provider: ${provider}. Supported providers are: azure, openai`
      );
  }
}
