import { AzureChatOpenAI, ChatOpenAI } from "@langchain/openai";

export type AIProvider = "azure" | "openai";
export type AzureModel = "gpt-4.1" | "gpt-4.1-nano" | "gpt-4.1-mini";
export type OpenAIModel = "gpt-4.1" | "gpt-4o";
export type ModelString = `azure:${AzureModel}` | `openai:${OpenAIModel}`;

export interface ModelOptions {
  temperature?: number;
  timeout?: number;
  maxRetries?: number;
}

const DEFAULT_OPTIONS: Record<AIProvider, ModelOptions> = {
  azure: {
    temperature: 0,
    timeout: 5000, // 5 second timeout
    maxRetries: 2,
  },
  openai: {
    temperature: 0,
    timeout: 5000, // 5 second timeout
    maxRetries: 2,
  },
};

export function getAIModel(modelString: ModelString, options?: ModelOptions) {
  if (!modelString.includes(":")) {
    throw new Error(
      `Invalid model string format. Expected "provider:model", got "${modelString}"`
    );
  }

  const [provider, model] = modelString.split(":") as [string, string];

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
        temperature: mergedOptions.temperature,
        timeout: mergedOptions.timeout,
        maxRetries: mergedOptions.maxRetries,
      });

    case "openai":
      return new ChatOpenAI({
        model: model,
        temperature: mergedOptions.temperature,
        timeout: mergedOptions.timeout,
        maxRetries: mergedOptions.maxRetries,
      });

    default:
      throw new Error(
        `Unsupported AI provider: ${provider}. Supported providers are: azure, openai`
      );
  }
}
