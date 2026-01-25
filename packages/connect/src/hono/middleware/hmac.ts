import crypto from "crypto";
import { createMiddleware } from "hono/factory";
import { HTTPException } from "hono/http-exception";
import { generateHmac, generateMessage } from "../../client/hmac";
import { registerNonce } from "../../util/replayProtection";

type GetSigningKey = (connectionId: string) => Promise<string>;

const TIMESTAMP_WINDOW_SECONDS = 300; // 5 minutes

export function createHmacMiddleware(getSigningKey: GetSigningKey) {
  return createMiddleware(async (c, next) => {
    const connectionId = c.req.header("inconvo-connection-id");
    if (!connectionId) {
      throw new HTTPException(400, { message: "Missing connection ID" });
    }

    const signature = c.req.header("inconvo-signature");
    const timestamp = c.req.header("inconvo-timestamp");
    const random = c.req.header("inconvo-random");

    if (!signature || !timestamp || !random) {
      throw new HTTPException(401, { message: "Missing auth headers" });
    }

    // Validate timestamp
    const requestTimestamp = Number(timestamp);
    const currentTimestamp = Math.floor(Date.now() / 1000);

    if (!Number.isFinite(requestTimestamp)) {
      throw new HTTPException(401, { message: "Invalid timestamp" });
    }

    if (Math.abs(requestTimestamp - currentTimestamp) > TIMESTAMP_WINDOW_SECONDS) {
      throw new HTTPException(401, { message: "Timestamp out of range" });
    }

    // Get request body for POST requests
    let body: object | undefined;
    let rawBody: string | undefined;
    if (c.req.method === "POST") {
      rawBody = await c.req.text();
      try {
        body = JSON.parse(rawBody);
      } catch {
        throw new HTTPException(400, { message: "Invalid JSON body" });
      }
    }

    // Generate expected signature
    const signingKey = await getSigningKey(connectionId);
    const message = generateMessage({
      method: c.req.method,
      requestUrl: c.req.path,
      timestamp,
      random,
      body,
    });
    const expectedSignature = generateHmac(message, signingKey);

    // Validate signature format
    const hexPattern = /^[0-9a-f]+$/i;
    if (signature.length !== expectedSignature.length || !hexPattern.test(signature)) {
      throw new HTTPException(401, { message: "Invalid signature format" });
    }

    // Timing-safe comparison
    const expectedBuffer = Buffer.from(expectedSignature, "hex");
    const receivedBuffer = Buffer.from(signature, "hex");

    if (!crypto.timingSafeEqual(expectedBuffer, receivedBuffer)) {
      throw new HTTPException(401, { message: "Invalid signature" });
    }

    // Replay protection
    const nonceAccepted = registerNonce(random, requestTimestamp, {
      nowSeconds: currentTimestamp,
    });

    if (!nonceAccepted) {
      throw new HTTPException(401, { message: "Replay detected" });
    }

    // Store parsed data for handlers
    c.set("connectionId", connectionId);
    if (body) {
      c.set("parsedBody", body);
    }

    await next();
  });
}
