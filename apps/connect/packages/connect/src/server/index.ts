import assert from "assert";
import { answer } from "~/types/types";

export async function ask(
  question: string,
  requestContext: Record<string, string | number>
) {
  const endpoint = "http://localhost:3000/api/ask";
  const inconvoApiKey = process.env.INCONVO_API_KEY;
  assert(inconvoApiKey, "Inconvo API key is not set");

  const payload = {
    question,
    requestContext,
  };

  try {
    const response = await fetch(endpoint, {
      method: "POST",
      headers: {
        "Content-Type": "application/json",
        Authorization: `Bearer ${inconvoApiKey}`,
      },
      body: JSON.stringify(payload),
    });

    if (!response.ok) {
      const errorText = await response.text();
      throw new Error(`Error: ${response.status} - ${errorText}`);
    }

    const data = (await response.json()) as answer;
    return data;
  } catch (error) {
    console.error("Error calling the API:", error);
    throw error;
  }
}

const inconvo = {
  ask,
};

export default inconvo;
