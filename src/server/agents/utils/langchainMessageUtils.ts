import type { AIMessage } from "@langchain/core/messages";

export function aiMessageContainsJsonLikeText(
  msg: AIMessage | undefined
): boolean {
  if (!msg) return false;
  const content = msg.content;
  let text = "";
  if (typeof content === "string") {
    text = content;
  } else if (Array.isArray(content)) {
    text = content
      .map((c) => {
        if (typeof c === "string") return c;
        if (c && typeof c === "object" && "text" in c) return c.text as string;
        return "";
      })
      .join("\n");
  }
  if (!text.includes("{") || !text.includes("}")) return false;
  const jsonLike = /\{[\s\S]*?:[\s\S]*?\}/.test(text);
  if (!jsonLike) return false;
  return true;
}

export function extractTextFromMessage(message: AIMessage): string[] {
  if (!message) return [""];

  const { content } = message;

  if (typeof content === "string") {
    return [content];
  }

  if (!Array.isArray(content)) {
    return [""];
  }

  const textMessages = content
    .filter((block) => block.type === "text")
    .map((block) => {
      const textBlock = block.text as string;
      return textBlock;
    });

  return textMessages;
}
