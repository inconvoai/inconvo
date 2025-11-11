import crypto from "crypto";
import { NextFunction, Request, Response } from "express";
import { generateHmac, generateMessage } from "~/util/hmac";
import { registerNonce } from "~/util/replayProtection";

export function authenticated(req: Request, res: Response, next: NextFunction) {
  const signature = req.headers["inconvo-signature"] as string | undefined;
  const timestamp = req.headers["inconvo-timestamp"] as string | undefined;
  const random = req.headers["inconvo-random"] as string | undefined;

  if (!signature || !timestamp || !random) {
    console.error("Invalid Request");
    res.status(401).json({ message: "Invalid Request" });
    return;
  }

  const message = generateMessage({
    method: req.method,
    body: req.body,
    requestUrl: req.url,
    timestamp,
    random,
  });

  const generatedSignature = generateHmac(message);
  const requestTimestamp = Number(timestamp);
  const currentTimestamp = Math.floor(Date.now() / 1000);

  if (!Number.isFinite(requestTimestamp)) {
    console.error("Unauthorized - invalid timestamp");
    res.status(401).json({ message: "Unauthorized" });
    return;
  }

  if (Math.abs(requestTimestamp - currentTimestamp) > 300) {
    console.error("Unauthorized - timestamp is too old or too new");
    res
      .status(401)
      .json({ message: "Unauthorized - timestamp is too old or too new" });
    return;
  }

  const expectedLength = generatedSignature.length;
  const hexPattern = /^[0-9a-f]+$/i;

  if (signature.length !== expectedLength || !hexPattern.test(signature)) {
    console.error("Unauthorized - invalid signature format");
    res.status(401).json({ message: "Unauthorized" });
    return;
  }

  const generatedBuffer = Buffer.from(generatedSignature, "hex");
  const receivedBuffer = Buffer.from(signature, "hex");

  if (!crypto.timingSafeEqual(generatedBuffer, receivedBuffer)) {
    console.error("Unauthorized");
    res.status(401).json({ message: "Unauthorized" });
    return;
  }

  const nonceAccepted = registerNonce(random, requestTimestamp, {
    nowSeconds: currentTimestamp,
  });

  if (!nonceAccepted) {
    console.error("Unauthorized - replay detected");
    res.status(401).json({ message: "Unauthorized - replay detected" });
    return;
  }

  next();
}
