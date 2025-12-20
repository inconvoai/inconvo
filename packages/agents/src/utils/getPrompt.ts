import * as hub from "langchain/hub";
import type { Runnable } from "@langchain/core/runnables";

const cache = new Map<string, { prompt: Runnable; timestamp: number }>();
const ONE_DAY_IN_MS = 24 * 60 * 60 * 1000;

export async function getPrompt(promptName: string): Promise<Runnable> {
  const now = Date.now();
  const cachedItem = cache.get(promptName);

  if (cachedItem && now - cachedItem.timestamp < ONE_DAY_IN_MS) {
    return cachedItem.prompt;
  }

  try {
    const prompt = await hub.pull(promptName);
    cache.set(promptName, { prompt, timestamp: now });
    return prompt;
  } catch (error) {
    console.error(`Error fetching prompt "${promptName}":`, error);
    throw error;
  }
}
