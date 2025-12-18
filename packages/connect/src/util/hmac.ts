import crypto from "crypto";
import { env } from "~/env";

interface MessageInput {
  method: string;
  requestUrl: string;
  body?: object;
  timestamp: string;
  random: string;
}

export function generateHmac(message: string): string {
  const algorithm = "sha256";
  const encoding = "hex";
  const secretKey = env.INCONVO_SECRET_KEY;
  const hmac = crypto.createHmac(algorithm, secretKey);
  hmac.update(message);
  const signature = hmac.digest(encoding);
  return signature;
}

export function generateMessage(input: MessageInput): string {
  return JSON.stringify({
    method: input.method,
    requestUrl: input.requestUrl,
    body: input.body ?? {},
    timestamp: input.timestamp,
    random: input.random,
  });
}
